# References Pages

<!-- vim-markdown-toc GFM -->

* [global](#global)
	* [token](#token)
	* [admin](#admin)
* [pastebin](#pastebin)
	* [enable](#enable)
	* [key](#key)
* [runner](#runner)
	* [name](#name)
	* [host](#host)
		* [address](#address)
		* [key](#key-1)
	* [admins](#admins)
* [default_config](#default_config)
	* [name](#name-1)
	* [link](#link)
	* [chat](#chat)
	* [admins](#admins-1)
	* [default_runner](#default_runner)
	* [interval](#interval)

<!-- vim-markdown-toc -->

## global

Here store the bot setting

### token

telegram bot token

### admin

id that who can modify bot settings

## pastebin

https://pastebin.com setting

### enable

if enable is true, paste result to pastebin

### key

api key for pastebin.com

## runner

contain list of runner

### name

runner name

### host

runner host

#### address

runner host ip address, should have http or https prefix.

#### key

OAuth key

### admins

list of admins that can access this runner

## default_config

Here store the default speed test configuration for schedule jobs

### name

config name for specific in schedule jobs.

### link

subscriptions link

### chat

where to send the result

### admins

list of users who can use this config to start a test

### default_runner

specific runner to run this config

### interval

pending for how many seconds after one schedule jobs done.

