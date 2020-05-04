# bot.py
import asyncio
import os
import random
from settings import config
import discord


TOKEN = config['DISCORD_TOKEN']


client = discord.Client()


class Game:
    pass


class Command:
    pass


class DB:
    pass


#<Message id=706997190320717954 channel=<TextChannel id=571090859144249376 name='general' position=0 nsfw=False news=False category_id=571090859144249375> type=<MessageType.default: 0> author=<Member id=136917603447865344 name='TestJockey' discriminator='1952' bot=False nick=None guild=<Guild id=571090859144249374 name='Jodran' shard_id=None chunked=True member_count=2>> flags=<MessageFlags value=0>>


class MessageHandler:
    def should_handle(self, message):
        raise NotImplementedError()

    def get_all_messages(self, message):
        raise NotImplementedError()


class MentionMessageHandler(MessageHandler):
    keyword = None
    
    def should_handle(self, message):
        mention, remainder = split_by_first_mention(message)
        return is_bot_mention(mention) and remainder.startswith(self.keyword)


class StatusHandler(MentionMessageHandler):
    keyword = 'status'
        
    def get_all_messages(self, message):
        return ['Bot alive']



message_handlers = [
    # StatusHandler()  # :weary:
]


async def handle_message(message):
    for message_handler in message_handlers:
        if message_handler.should_handle(message):
            messages = message_handler.get_all_messages(message)
            for message in messages:
                await message.channel.send(msg)



@client.event
async def on_message(message):
    if message.author == client.user:
        return

    await handle_message(message)


if __name__ == "__main__":
    client.run(TOKEN)
