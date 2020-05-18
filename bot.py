import time
import os
import discord
from handlers import get_message_handlers
from dotenv import load_dotenv

load_dotenv()

TOKEN = os.getenv('DISCORD_TOKEN')
CLIENT_ID = os.getenv('CLIENT_ID')

client = discord.Client()

# <Message id=706997190320717954 channel=<TextChannel id=571090859144249376 name='general' position=0 nsfw=False news=False category_id=571090859144249375> type=<MessageType.default: 0> author=<Member id=136917603447865344 name='TestJockey' discriminator='1952' bot=False nick=None guild=<Guild id=571090859144249374 name='Jodran' shard_id=None chunked=True member_count=2>> flags=<MessageFlags value=0>>

async def handle_message(message):
    for message_handler in get_message_handlers():
        if message_handler.should_handle(message):
            responses = message_handler.get_all_responses(message)
            for response in responses:
                print(time.time(), 'Responding with: "%s"' % response)
                if type(response) == str:
                    await message.channel.send(response)
                else:
                    await response.send()


@client.event
async def on_message(message):
    if message.author == client.user:
        return
    await handle_message(message)


if __name__ == "__main__":
    client.run(TOKEN)
