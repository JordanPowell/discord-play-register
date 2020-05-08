import os
import unittest
from discord.message import Message
from discord.enums import MessageType
from discord.user import User
from bot import handle_message
import asyncio


class FakeState:
    def store_user(self, user):
        pass


def create_discord_user(**kwargs):
    default_params = {
        'id': 1,
        'discriminator': None,
        'avatar': None,
        'username': 'Test user'
    }
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
        self.bot_responses = []

    def _preprocess_test_discord_message(self, msg):
        return msg.replace('@bot', '<@!%s>' % os.getenv('CLIENT_ID'))

    def _create_fake_discord_message(self, content, **kwargs):
        class FakeChannel:
            async def send(inner_self, msg):
                self.bot_responses.append(msg)

        return create_discord_message(content, channel=FakeChannel(), **kwargs)

    def user_message(self, message):
        """Use @bot for mentions"""
        coroutine = handle_message(self._create_fake_discord_message(self._preprocess_test_discord_message(message)))
        asyncio.get_event_loop().run_until_complete(coroutine)

    def assertNextBotMessagesContains(self, *fragments):
        self.assertEqual(len(fragments), len(self.bot_responses))
        for expected_fragment, bot_response in zip(fragments, self.bot_responses):
            self.assertIn(expected_fragment, bot_response)
        self.bot_responses = []


class TestStatus(PlayRegisterBotTestCase):
    def test_status_line_no_games(self):
        self.user_message('@bot status')
        self.assertNextBotMessagesContains('Bot alive')

