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


function debugLog(msg) {
  if (auth.debug === true) {
    console.log(msg);
  }
}

const thisMention = `<@!${auth.client_id}>`;

bot.on('message', function (user, userID, channelID, message, evt) {
  if (userID != auth.client_id) {
    
    if (auth.debug === true) {
      debugLog('Debug: Message=', message);
    }
    var start = new Date();

    const ps = spawn(path.join(__dirname, 'bot.sh'));
    
    ps.stdout.on('data', (data) => {
      bot.sendMessage({
        to: channelID,
        message: data
      });
    });

    ps.stderr.on('data', (data) => {
      console.error(`subproc stderr: ${data}`);
    });

    ps.on('close', (code) => {
      debugLog(`Runtime took ${new Date() - start}`);
    });
    
    ps.stdin.write(message);
    ps.stdin.end();
  }
});
       
console.log('Started');
