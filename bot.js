const path = require('path');
const { spawn } = require('child_process');
var Discord = require('discord.io');
var logger = require('winston');
var auth = require('./auth.json');


// Initialize Discord Bot
var bot = new Discord.Client({
  token: auth.token,
  autorun: true
});


const thisMention = `<@!${auth.client_id}>`;

bot.on('message', function (user, userID, channelID, message, evt) {
  console.log('Debug: Message=', message);
  // Our bot needs to know if it will execute a command
  // It will listen for messages that will start with `!`
  if (message.includes(thisMention)) {
    const ps = spawn(path.join(__dirname, 'bot.sh'));
    
    ps.stdout.on('data', (data) => {
      console.log(`subproc stdout: ${data}`);
    });

    ps.stderr.on('data', (data) => {
      console.error(`subproc stderr: ${data}`);
    });

    ps.stdin.write(message);
    ps.stdin.end();
    
    bot.sendMessage({
      to: channelID,
      message: 'Pong!'
    });
  }
});
      

