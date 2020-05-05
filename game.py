from db import db

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
    KnownGame(name='League of Legends', aliases=['league','lol','xlol'], min_players=2, max_players=5),
    KnownGame(name='Test', aliases=['tst'], min_players=1, max_players=1)
]

def lookup_game_by_name_or_alias(name):
    # Name may contain extra junk, e.g. "I'd play cs later, after food" would mean name="cs later, after food"
    for game in games:
        if game.loosely_matches(name):
            return game
    return Game(name=name)
