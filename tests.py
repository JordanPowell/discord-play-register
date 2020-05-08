from game import lookup_game_by_name_or_alias, get_known_games
import os
from db import db
import unittest
from discord.message import Message
from discord.enums import MessageType
from discord.user import User
from bot import handle_message
import asyncio


class FakeState:
    def store_user(self, user):
        return user


_user_id = 1


def create_discord_user(**kwargs):
    global _user_id
    default_params = {
        'id': _user_id,
        'discriminator': None,
        'avatar': None,
        'username': 'Test user %s' % _user_id
    }
    _user_id += 1
    default_params.update(kwargs)
    return User(state=FakeState(), data=default_params)


def create_discord_message(content, *, channel, **kwargs):
    default_params = {
        'content': content,
        'id': 1,
        'attachments': [],
        'embeds': [],
        'edited_timestamp': 0,
        'pinned': False,
        'mention_everyone': False,
        'tts': False,
        'mentions': [],
        'type': MessageType.default,
        'author': create_discord_user()
    }
    default_params.update(kwargs)
    message = Message(state=FakeState(), channel=channel, data=default_params)
    return message


class PlayRegisterBotTestCase(unittest.TestCase):
    def setUp(self):
        super().setUp()
        self.old_games_db_file = os.environ.get('GAMES_DB_FILE')
        os.environ['GAMES_DB_FILE'] = 'test_known_games.json'
            
        for game in get_known_games():
            db.clear_game(game)
        self.bot_responses = []

    def tearDown(self):
        if self.old_games_db_file is not None:
            os.environ['GAMES_DB_FILE'] = self.old_games_db_file     

    def _preprocess_test_discord_message(self, msg):
        return msg.replace('@bot', '<@!%s>' % os.getenv('CLIENT_ID'))

    def _create_fake_discord_message(self, content, **kwargs):
        class FakeChannel:
            async def send(inner_self, msg):
                self.bot_responses.append(msg)

        return create_discord_message(content, channel=FakeChannel(), **kwargs)

    def user_message(self, message, author=create_discord_user()):
        """Use @bot for mentions"""
        coroutine = handle_message(self._create_fake_discord_message(self._preprocess_test_discord_message(message), author=author))
        asyncio.get_event_loop().run_until_complete(coroutine)

    def assertNextBotMessagesContains(self, *fragments):
        self.assertEqual(len(fragments), len(self.bot_responses))
        for expected_fragment, bot_response in zip(fragments, self.bot_responses):
            self.assertIn(expected_fragment, bot_response)
        self.bot_responses = []

    def assertNumPlayersForGame(self, game, num):
        self.assertEqual(num, len(lookup_game_by_name_or_alias(game).get_available_players()))


class TestStatus(PlayRegisterBotTestCase):
    def test_status_line_no_games(self):
        self.user_message('@bot status')
        self.assertNextBotMessagesContains('Bot alive')


class TestSame(PlayRegisterBotTestCase):
    def test_same_works_without_game_single_game(self):
        self.user_message("I'd play cs")
        self.user_message("same", author=create_discord_user())
        self.assertNumPlayersForGame('cs', 2)

    def test_same_works_with_game_single_game(self):
        self.user_message("I'd play cs")
        self.user_message("same to cs", author=create_discord_user())
        self.assertNumPlayersForGame('cs', 2)

    def test_same_works_without_game_multiple_game(self):
        self.user_message("I'd play cs/rl")
        self.user_message("same", author=create_discord_user())
        self.assertNumPlayersForGame('cs', 2)
        self.assertNumPlayersForGame('rl', 2)

    def test_same_works_with_game_multiple_game(self):
        self.user_message("I'd play cs/rl")
        self.user_message("same to cs", author=create_discord_user())
        self.assertNumPlayersForGame('cs', 2)
        self.assertNumPlayersForGame('rl', 1)
