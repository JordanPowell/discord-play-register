from db import db
from utils import extract_remainder_after_fragments, extract_time
from game import lookup_game_by_name_or_alias, get_known_games, lookup_known_game_by_name_or_alias, \
    write_games_dict, read_games_dict, create_mention
from dotenv import load_dotenv
import itertools
import os


load_dotenv()


CLIENT_ID = os.getenv('CLIENT_ID')


def get_message_handlers():
    return [
        WouldPlayHandler(),
        ClearHandler(),
        CancelHandler(),
        PingHandler(),
        AccidentalRoleMentionHandler(),
        StatusHandler(),
        QueryHandler(),
        AddHandler(),
        HelpHandler()
    ]


def get_any_ready_messages(game):
    if game.is_ready_to_play:
        return game.get_ready_messages()
    return []


def replace_last_occurence(original, string_to_replace, replace_with):
    return replace_with.join(original.rsplit(string_to_replace, 1))


def make_sentence_from_strings(string_list):
    return replace_last_occurence(", ".join(string_list), ", ", " and ")


def split_by_first_mention(message):
    msg = message.content
    if msg.startswith('<@'):
        idx = msg.index('>') + 1
        return msg[:idx], msg[idx:].strip()
    else:
        return '', msg


def message_starts_with_any_fragment(message, fragments):
    return any(message.lower().startswith(query_game_fragment.lower())
               for query_game_fragment in fragments)


def message_pertains_to_all_games(message):
    query_game_fragments = ['games', 'game', 'list', 'g']
    return message_starts_with_any_fragment(message, query_game_fragments)


def get_game_name_or_alias_from_message(message):
    if lookup_game_by_name_or_alias(message):
        game = lookup_game_by_name_or_alias(message)
        for potential_name in [game.name] + game.aliases:
            if message.lower().startswith(potential_name.lower()):
                return potential_name


def get_default_game_dict_representation(game_name):
    return {
        game_name:
            {
                'aliases': [],
                'max_players': 5,
                'min_players': 2
            }
    }


def is_bot_mention(mention):
    return mention[3 if mention.startswith('<@!') else 2:-1] == CLIENT_ID


class GameExtractionMixin:
    multi_game_delimiter = '/'

    def get_all_responses_with_games(self, message, games):
        responses = []
        for game in games:
            responses += self.get_all_responses_with_game(message, game) if game else []
        return responses

    def get_all_responses_without_game(self, message):
        return []

    def get_all_responses_with_game(self, message, game):
        return []

    def get_all_responses(self, message):
        plays = extract_remainder_after_fragments(self.fragments, message.content)
        game_names = plays.split(self.multi_game_delimiter)
        games = [lookup_game_by_name_or_alias(game_name) for game_name in game_names if lookup_game_by_name_or_alias(game_name)]
        responses = []
        if games:
            games = [game for game in games if game]
            responses += self.get_all_responses_with_games(message, games)
        else:
            responses += self.get_all_responses_without_game(message)
        return responses


class MessageHandler:
    helper_command_list = []

    def should_handle(self, message):
        raise NotImplementedError()

    def get_all_responses(self, message):
        raise NotImplementedError()


class ContentBasedHandler(MessageHandler):
    fragments = []

    def should_handle(self, message):
        return any(message.content.lower().startswith(f.lower()) for f in self.fragments)


class MentionMessageHandler(MessageHandler):
    keywords = []

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fragments = self.keywords

    def should_handle(self, message):
        mention, remainder = split_by_first_mention(message)
        return is_bot_mention(mention) and any(remainder.lower().startswith(kw.lower()) for kw in self.keywords)

    def split_string_by_keywords(self, string):
        for keyword in self.keywords:
            kw_removed_string = string.replace(keyword, '', 1)

            if kw_removed_string != string:
                kw_removed_string = kw_removed_string.strip()
                return keyword, kw_removed_string

        return None, string


class WouldPlayHandler(GameExtractionMixin, ContentBasedHandler):
    fragments = ["I'd play", "id play", "I'd paly", "id paly", "I’dplay", "I’dpaly", "same to", "same"]
    helper_command_list = [f"{fragments[0]} <game> - Add your name to the list of players that would play <game>."]

    def get_all_responses_with_games(self, message, games):
        for_time = extract_time(message.content)
        game_and_players_strings = []
        for game in games:
            db.record_would_play(message.author, game, for_time)
        game_and_players_strings = ["%s (%s)" % (game.name, len(game.get_available_players())) for game in games]
        messages = ["%s would play %s" % (message.author.display_name, make_sentence_from_strings(game_and_players_strings))]
        for game in games:
            messages += get_any_ready_messages(game)
        return messages

        messages = ["%s would play %s" % (message.author.display_name, make_sentence_from_strings(game_and_players_strings))]

        for game in games:
            messages += get_any_ready_messages(game)
        return messages

    def get_all_responses_without_game(self, message):
        last_would_plays = db.get_last_would_plays_at_same_time()

        if not last_would_plays:
            return []

        games = set([lwp.game for lwp in last_would_plays])
        messages = self.get_all_responses_with_games(message, games)

        return messages


class StatusHandler(MentionMessageHandler):
    keywords = ['status']
    helper_command_list = [f"@BOT {keywords[0]} - allow the user to determine the status of games (number of players etc.)."]

    def get_all_responses(self, message):
        messages = ['Bot alive']
        for game in get_known_games():
            players = game.get_ready_players()
            if players:
                messages.append('%s has %s (%s)' % (game, len(players), ", ".join([player.display_name for player in players])))
        return ['\n'.join(messages)]


class ClearHandler(GameExtractionMixin, MentionMessageHandler):
    keywords = ['clear']
    helper_command_list = [f"@BOT {keywords[0]} <game> - clear all names from the <game>'s' \"I'd play\" list."]

    def get_all_responses_with_game(self, message, game):
        if game:
            db.clear_game(game)
            return ['Cleared %s' % game]
        else:
            return ['No game specified!']


class CancelHandler(MentionMessageHandler):
    keywords = ['cancel']
    helper_command_list = [f"@BOT {keywords[0]} - Removes your name from all \"I'd play\" lists."]

    def get_all_responses(self, message):
        db.cancel_would_plays(message.author)
        return ['Cancelled all play requests from %s' % (message.author.display_name)]


class PingHandler(GameExtractionMixin, MentionMessageHandler):
    keywords = ['ping', 'p']
    helper_command_list = [f"@BOT {keywords[0]} <game> - Ping all players that would currently play <game> and refresh the list for <game>."]

    def get_all_responses_with_game(self, message, game):
        players = game.get_players_for_next_game()
        db.clear_game(game)
        return ['%s - ready to play %s.' % (','.join([create_mention(p) for p in players]), game)]


class AccidentalRoleMentionHandler(MessageHandler):
    def should_handle(self, message):
        return 'Play Register' in message.clean_content and '<@&' in message.content

    def get_all_responses(self, message):
        return ['It looks like you tried to @ me but might have accidentally selected the role instead']


class QueryHandler(MentionMessageHandler):
    keywords = ['query']
    helper_command_list = [f"@BOT {keywords[0]} games - Determine what games are in the known_games list.",
                           f"@BOT {keywords[0]} <property> <game> - Determine the <property> value for <game> (i.e. min_players)."]

    def get_all_responses(self, message):
        mention, remainder = split_by_first_mention(message)
        found_keyword, remainder = self.split_string_by_keywords(remainder)

        if message_pertains_to_all_games(remainder):
            return ['\n'.join([game.name for game in get_known_games()])]
        else:
            attribute, game_name = remainder.split(' ')[:2]
            game = lookup_game_by_name_or_alias(game_name)
            attribute_display = {
                'aliases': lambda z: ', '.join([alias for alias in z])
            }
            display_function = attribute_display.get(attribute, lambda x: str(x))
            return ["%s: %s" % (attribute, display_function(getattr(game, attribute)))]


class AddHandler(MentionMessageHandler):
    """ Called via '@bot add game <game>' or '@bot add <property> <game> <value>' """
    keywords = ['add']
    json_filename = os.path.join(os.path.dirname(__file__), 'known_games.json')

    helper_command_list = [f"@BOT {keywords[0]} <games> - Add <game> to the known_games list.",
                           f"@BOT {keywords[0]} <property> <game> <value> - Add <value> to the <property> of <game> (Edits min/max_players)."]

    def get_all_responses(self, message):
        mention, remainder = split_by_first_mention(message)
        found_keyword, remainder = self.split_string_by_keywords(remainder)
        split_remainder = remainder.split(' ')
        if len(split_remainder) == 1:
            return ["Incorrect command: Try 'add game <game name>' or 'add <property> <game name> <value>"]

        if message_pertains_to_all_games(split_remainder[0]):
            new_game = ' '.join(split_remainder[1:])

            if lookup_known_game_by_name_or_alias(new_game):
                return ["That game already exists you absolute degenerate. Don't trigger me."]
            else:
                new_game_dict = get_default_game_dict_representation(new_game)
                known_games_dict = read_games_dict()
                known_games_dict.update(new_game_dict)
                write_games_dict(known_games_dict)
                return ["Congratulations - %s has been added to the known games list! Fantastic work there comrade, give yourself a pat on the back!" % new_game]
        else:
            property, remainder = split_remainder[0], ' '.join(split_remainder[1:])
            if get_game_name_or_alias_from_message(remainder):
                game_name = get_game_name_or_alias_from_message(remainder).lower()
            else:
                return ["Invalid game name/ alias"]

            game = lookup_game_by_name_or_alias(game_name)
            value = remainder[len(game_name) + 1:]

            known_games_dict = read_games_dict()
            if property.lower() == 'alias':
                known_games_dict[game.name]['aliases'] += [value]
            elif property.lower() in ['min_players', 'max_players']:
                known_games_dict[game.name][property] = int(value)
            else:
                return ["Invalid property type"]

            write_games_dict(known_games_dict)
            return ["%s has been added to %s in %s" % (value, property, game.name)]


def add_commands_to_command_helper_list():
    return ['\n'.join(f"`{cmd.split('-')[0]}` - {cmd.split('-')[1]}" for cmd in itertools.chain.from_iterable(handler.helper_command_list for handler in get_message_handlers()))]


class HelpHandler(MentionMessageHandler):
    keywords = ['help', 'h', '?']

    def get_all_responses(self, message):
        return add_commands_to_command_helper_list()
