import time
import asyncio
import os
import random
from settings import config
import discord


TOKEN = config['DISCORD_TOKEN']


client = discord.Client()

DEFAULT_EXPIRY_S = 60 * 60 * 4


class WouldPlay:
    def __init__(self, player, game, expires_at=None):
        self.player = player
        self.game = game
        self.recorded_at = time.time()
        self.expires_at = expires_at or (self.recorded_at + DEFAULT_EXPIRY_S)

    @property
    def user(self):
        return self.player.name

    @property
    def expired(self):
        return self.expires_at <= time.time()
        
    def __eq__(self, other):
        if type(other) is type(self):
            return (self.player.id == other.player.id and self.game.name == other.game.name)
        else:
            return False

    def __hash__(self):
        return hash((self.player.id, self.game.name))


class DB:
    def __init__(self):
        self._store = set()

    def record_would_play(self, player, game):
        if not game.name:
            raise RuntimeError('Cannot record for a game with no name')
        wp = WouldPlay(player=player, game=game)
        self._store.add(wp)
        self._prune_expired()
        return wp

    def get_players_for_game(self, game):
        return [wp.player for wp in self.get_would_plays_for_game(game)]

    def get_would_plays_for_game(self, game):
        self._prune_expired()
        return sorted([wp for wp in self._store if wp.game.name == game.name], key=lambda x: x.recorded_at)

    def _prune_expired(self):
        # why can't I do self.prune(wp -> wp.expired)
        self._store = set([wp for wp in self._store if not wp.expired])

    def clear_game(self, game):
        self._store = set([wp for wp in self._store if wp.game.name != game.name])


db = DB()


def get_any_ready_messages(game):
    if game.is_ready_to_play:
        return game.get_ready_messages()
    return []

class Game:
    def __init__(self, name, aliases=[], min_players=0, max_players=100, known=False):
        self.name = name
        self.aliases = aliases
        self.min_players = min_players
        self.max_players = max_players
        self.known = known

    def loosely_matches(self, string_with_game_name):
        return any(string_with_game_name.lower().startswith(potential_name.lower())
                   for potential_name in [self.name] + (self.aliases or []))

    def get_available_players(self):
        return db.get_players_for_game(self)

    def get_players_for_next_game(self):
        return self.get_available_players()[:self.max_players]
    
    def is_ready_to_play(self):
        players = self.get_available_players()
        return len(players) >= self.min_players

    def get_ready_messages(self):
        players = self.get_available_players()
        if len(players) >= self.min_players:
            if len(players) >= self.max_players:
                return ["Ready to play! %s would play %s.\n@ me with 'clear %s' to clear the players, or with 'ping %s' to @ the players and clear." % (
                    ', '.join([p.name for p in players]),
                    self.name,
                    self.name,
                    self.name)]
            else:
                return ["Potentially ready to play with %s! %s would play %s.\n@ me with 'clear %s' to clear the players, or with 'ping %s' to @ the players and clear." % (
                    len(players),
                    ', '.join([p.name for p in players]),
                    self.name,
                    self.name,
                    self.name)]
        return []
    
    def __str__(self):
        return '%s%s' % (self.name, ' (unknown game)' if not self.known else '')
    
class KnownGame(Game):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, known=True, **kwargs)
    
games = [
    KnownGame(name='CS', aliases=['csgo', 'counterstrike', 'cs:go'], min_players=5, max_players=5),
    KnownGame(name='Rocket League', aliases=['rl', '3s', '2s'], min_players=2, max_players=3),
    KnownGame(name='Valorant', aliases=['valorant'], min_players=3, max_players=5),
    KnownGame(name='Test', aliases=['tst'], min_players=1, max_players=1)
]

def lookup_game_by_name_or_alias(name):
    # Name may contain extra junk, e.g. "I'd play cs later, after food" would mean name="cs later, after food"
    for game in games:
        if game.loosely_matches(name):
            return game
    return Game(name=name)

#<Message id=706997190320717954 channel=<TextChannel id=571090859144249376 name='general' position=0 nsfw=False news=False category_id=571090859144249375> type=<MessageType.default: 0> author=<Member id=136917603447865344 name='TestJockey' discriminator='1952' bot=False nick=None guild=<Guild id=571090859144249374 name='Jodran' shard_id=None chunked=True member_count=2>> flags=<MessageFlags value=0>>


def split_by_first_mention(message):
    msg = message.content
    if msg.startswith('<@'):
        idx = msg.index('>') + 1
        return msg[:idx], msg[idx:].strip()
    else:
        return '', msg
        

def is_bot_mention(mention):
    return mention[3 if mention.startswith('<@!') else 2:-1] == config['CLIENT_ID']


def create_mention(player):
    return '<@!%s>' % player.id


class GameExtractionMixin:
    def get_all_responses(self, message):
        self.game_name = extract_remainder_after_fragments(self.fragments, message.content)        
        self.game = lookup_game_by_name_or_alias(self.game_name)
        return self.get_all_responses_with_game(message, self.game)

    
class MessageHandler:
    def should_handle(self, message):
        raise NotImplementedError()

    def get_all_responses(self, message):
        raise NotImplementedError()


class ContentBasedHandler(MessageHandler):
    fragments = []

    def should_handle(self, message):
        return any((f.lower() in message.content.lower() for f in self.fragments))

    
class MentionMessageHandler(MessageHandler):
    keyword = None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fragments = [self.keyword]
    
    def should_handle(self, message):
        mention, remainder = split_by_first_mention(message)
        return is_bot_mention(mention) and remainder.lower().startswith(self.keyword.lower())


def extract_remainder_after_fragments(fragments, content):
    content = content.lower()
    for fragment in fragments:
        fragment = fragment.lower()
        try:
            idx = content.index(fragment)
            return content[idx + len(fragment) + 1:]
        except ValueError:
            continue  # or pass?
    return content
    
    
class WouldPlayHandler(GameExtractionMixin, ContentBasedHandler):
    fragments = ["I'd play", "id play", "I'd paly", "id paly", "I’d play", "I’d paly", "I’dplay", "I’dpaly"]

    def get_all_responses_with_game(self, message, game):
        if self.game_name and any(message.content.lower().startswith(f.lower()) for f in self.fragments):
            would_play = db.record_would_play(message.author, game)
            return ["%s would play %s (that's %s)" % (would_play.user, game, len(game.get_available_players()))] + get_any_ready_messages(game)
        else:
            return []

    
class StatusHandler(MentionMessageHandler):
    keyword = 'status'
        
    def get_all_responses(self, message):
        messages = ['Bot alive']
        ready_messages = []
        for game in games:
            players = game.get_available_players()
            if players:
                messages.append('%s has %s' % (game, len(players)))
                ready_messages += get_any_ready_messages(game)
        return ['\n'.join(messages + ready_messages)]


class ClearHandler(GameExtractionMixin, MentionMessageHandler):
    keyword = 'clear'
    
    def get_all_responses_with_game(self, message, game):
        db.clear_game(game)
        return ['Cleared %s' % game]


class PingHandler(GameExtractionMixin, MentionMessageHandler):
    keyword = 'ping'
    
    def get_all_responses_with_game(self, message, game):
        players = game.get_players_for_next_game()
        db.clear_game(game)        
        return ['%s - ready to play %s.' % (','.join([create_mention(p) for p in players]), game)]

    
message_handlers = [
    WouldPlayHandler(),
    ClearHandler(),
    PingHandler(),
    StatusHandler()  # :weary:
]


async def handle_message(message):
    for message_handler in message_handlers:
        if message_handler.should_handle(message):
            responses = message_handler.get_all_responses(message)
            for response in responses:
                print(time.time(), 'Responding with: "%s"' % response)
                await message.channel.send(response)



@client.event
async def on_message(message):
    if message.author == client.user:
        return
    await handle_message(message)


if __name__ == "__main__":
    client.run(TOKEN)

