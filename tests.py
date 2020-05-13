from game import lookup_game_by_name_or_alias, get_known_games
import os
from db import db
import unittest
from discord.message import Message
from discord.enums import MessageType
from discord.user import User
from bot import handle_message
import asyncio
from utils import extract_time, format_time_string
from datetime import datetime


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

    def assertNumPlayersReadyForGame(self, game, num):
        self.assertEqual(num, len(lookup_game_by_name_or_alias(game).get_ready_players()))

    def assertNumPlayersUnreadyForGame(self, game, num):
        self.assertEqual(num, len(lookup_game_by_name_or_alias(game).get_unready_players()))


class UtilsTestCase(unittest.TestCase):
    def assert_format_time_string(self, time_string, formatted_string):
        self.assertEqual(format_time_string(time_string), formatted_string)

    def assert_extract_time(self, message, ftime):
        self.assertEqual(extract_time(message), ftime)


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


class TestTimeUtils(UtilsTestCase):
    def test_format_time_string(self):
        self.assert_format_time_string("5", "05:00pm")
        self.assert_format_time_string("5pm", "05:00pm")
        self.assert_format_time_string("10pm", "10:00pm")
        self.assert_format_time_string("10", "10:00pm")
        self.assert_format_time_string("10am", "10:00am")
        self.assert_format_time_string("5am", "05:00am")
        self.assert_format_time_string("5:01", "05:01pm")
        self.assert_format_time_string("5:00pm", "05:00pm")
        self.assert_format_time_string("05:00pm", "05:00pm")
        self.assert_format_time_string("5:00am", "05:00am")
        self.assert_format_time_string("05:00am", "05:00am")

    def test_extract_time(self):
        today = datetime.today()
        self.assert_extract_time("id play cs", None)
        self.assert_extract_time("id play cs at", None)
        self.assert_extract_time("id play cs @ 11", datetime(year=today.year, month=today.month, day=today.day, hour=23, minute=0).timestamp())
        self.assert_extract_time("id play cs at 11:59", datetime(year=today.year, month=today.month, day=today.day, hour=23, minute=59).timestamp())


class TestTime(PlayRegisterBotTestCase):
    def test_id_play_in_future(self):
        self.user_message("I'd play cs at 11:59pm")
        self.assertNumPlayersUnreadyForGame('cs', 1)
        self.assertNumPlayersReadyForGame('cs', 0)

    def test_some_id_play_in_future(self):
        self.user_message("I'd play cs")
        self.user_message("I'd play cs at 11:59pm", author=create_discord_user())
        self.user_message("I'd play cs @ 11:59pm", author=create_discord_user())
        self.user_message("I'd play cs at 00:01am", author=create_discord_user())
        self.assertNumPlayersUnreadyForGame('cs', 2)
        self.assertNumPlayersReadyForGame('cs', 2)

    def test_multi_id_play_in_future(self):
        self.user_message("I'd play cs/rl/lol")
        self.user_message("I'd play cs/rl/lol at 11:59pm", author=create_discord_user())
        self.user_message("I'd play cs/rl/lol @ 11:59pm", author=create_discord_user())
        self.user_message("I'd play cs/rl/lol at 00:01am", author=create_discord_user())
        self.assertNumPlayersUnreadyForGame('cs', 2)
        self.assertNumPlayersReadyForGame('cs', 2)
        self.assertNumPlayersUnreadyForGame('rl', 2)
        self.assertNumPlayersReadyForGame('rl', 2)
        self.assertNumPlayersUnreadyForGame('lol', 2)
        self.assertNumPlayersReadyForGame('lol', 2)
