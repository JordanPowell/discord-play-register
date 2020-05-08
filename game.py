from db import db
import json
import os


def create_mention(player):
    return '<@!%s>' % player.id


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
                return ["%s are ready to play %s!\n@ me with 'clear %s' to clear the players." % (
                    ','.join([create_mention(p) for p in players]),
                    self.name,
                    self.name,)]
        return []

    def __str__(self):
        return '%s%s' % (self.name, ' (unknown game)' if not self.known else '')

    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)


class KnownGame(Game):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, known=True, **kwargs)


def lookup_known_game_by_name_or_alias(name):
    for game in get_known_games():
        if game.loosely_matches(name):
            return game

def lookup_game_by_name_or_alias(name):
    # Name may contain extra junk, e.g. "I'd play cs later, after food" would mean name="cs later, after food"
    game = lookup_known_game_by_name_or_alias(name)
    return game if game else Game(name=name)


def read_games_dict(json_filename=os.path.join(os.path.dirname(__file__), 'known_games.json')):
    with open(json_filename) as json_file:
        return json.load(json_file)


def write_games_dict(known_games_json, json_filename=os.path.join(os.path.dirname(__file__), 'known_games.json')):
    with open(json_filename, "w") as json_file:
        json.dump(known_games_json, json_file, sort_keys=True, indent=4)


def get_known_games():
    known_game_dict = read_games_dict()
    return [KnownGame(name=name, **props) for name, props in known_game_dict.items()]
