# lizard-bot
A mastodon bot for animals (and people) to tag that replies with a how far a lizard would have to scuttle on a keyboard from key to key in order to type out the whole message.

## TODO
- [ ] get basic functionality working
- [x] respect #NoBot
- [x] prevent duplicate replies from different people tagging us (over <1 hour)
- [x] allow tagging on ourself & get fun message
- [ ] allow overrides to #NoBot
- [ ] customizable values per user
- [ ] weekly/monthly summaries for each user at request
- [ ] serialize/deserialize state for stopping/starting

## How to use

In order to use this bot, you first have to create an account with it. Just ask the bot to subscribe to you by sending a !subscribe command. This will make the bot periodically send you a summary of all your posts, either cumulatively or just since the last summary. For this method, you create and manage an account for the bot by using commands. You can also have the bot analyze individual posts by either mentioning us somewhere in the post, or mentioning us in a reply. If you have an account, you can also choose to let others call lizard-bot to analyze your posts.

This bot makes a distinction between users and accounts. Accounts are the things on instances that users log into to post things, and users are the animals (or people) who are doing the posting. By decoupling these two concepts, this model can do things like send a summary of a user's activity across multiple alternate accounts to a single primary account, or attribute a plural system's statuses to individual members by looking for a signature within the post.

## Basic Commands

### !subscribe
Usage: !subscribe [user (optional for accounts with 1 or fewer users)] [integer] [duration]

This subscribes you to receive summaries of your toots at a frequency of your choice. The duration you supply must be either days, weeks, or months. The bot will reply with a confirmation of your subscription, and send you a follow request. You don't need to accept the request for the bot to work properly, all it does is include followers only posts for slightly more accurate totals.

### !unsubscribe
Usage: !unsubscribe [user (optional for accounts with 1 user)]

This unsubscribes the user from receiving scheduled summaries. No data is deleted, and the user can still request summaries manually.

### !customize
Usage: 
	
	!customize [user (optional for accounts with 1 user)]
		[slot]: [value]
		[slot]: [value]
		etc.

Sets the each of the user's slots to the given value. Each assignment must be on it's own line. The list of possible slots to customize is:
* signature
* summary-visibility
* ignore-user
* animal
* verb
* keyboard-unit-size
* starting-position
* move-speed
* key-press-time
* keyboard-tilt
* step-size
* length-unit
* cumulative-stats

Or you can mention us with "!help !customize" or "!describe [user]" for this list. The bot will reply with a confirmation that the slots were set correctly, and let the user know if any errors occurred.

### !describe
Usage: !describe [user (optional for accounts with 1 user)]

Replies with a list of all slots that can be customized, and their current values for the user.

### !summarize-now
Usage: !summarize-now [user (optional for accounts with 1 user)]

Immediately replies with a summary for the user no matter when the next scheduled summary would be. The time until the next automatic summary is reset, so this command can be used to move summaries to a certain day of the week or month.

### !help
Usage: !help [(optionally one of the following) !command, slot]

When no command or slot is given, replies with a list of possible commands grouped into categories.
If a command is given, replies with a detailed description of how to use that command.
If a slot is given, replies with a detailed description of what the slot is for.

### !unlock
Usage: !unlock

This command unlocks your ACCOUNT (not user), which allows anyone to make lizard-bot post a summary of a status of yours by mentioning us in a reply. This is disabled by default (your account starts off locked). There isn't any way that lizard-bot publically displays whether your account is locked or not, so if you want others to know they can do this, you have to let them know yourself. An important thing to note is that if you have someone muted or blocked, but your lizard-bot account is unlocked and they mention lizard-bot in a reply, the bot will reply with a summary as normal since it has no way of knowing who you have blocked. This is why all accounts are locked by default.

### !lock
Usage: !lock

This command locks your ACCOUNT (not user), which prevents lizard-bot from replying to mentions with a summary.

## Accounts and Users

### !new-user
Usage: !new-user [name]

Creates a new user with the given name. The name can't be the same as a user that already exists, and can't contain whitespace characters. The new user is only associated with the account it was created from.

### !rename-user

Usage: !rename-user [current-name] [new-name]

Changes the name of user with current-name to new-name. The new-name can't be the same as a user that already exists with your account.

### !primary-account
Usage: !primary-account [user (optional for accounts with 1 user)] [account (optional)]

If [account] was supplied, this command sets the primary account of this user to [account], otherwise removes [account] as the primary account. When a summary is posted, only the primary-account will be mentioned.

### !fallback
Usage: !fallback [user]

Sets the default user (that posts without signatures are assumed to be sent by) to [user].

### !add-account
Usage:
	
	Account A:		@lizardlength !add-account [user] [account-b@instance.whatever]
	
	Account B in reply:	@lizardlength [account-a@instance.cool] !add-account

Creates an entry for Account B if it doesn't exist, then associates the account with the given user and vice versa. 

### !list-users
Usage: !list-users

This command will reply with a list of all users associated with this account, and for each user will include whther it is the default user for that account, the user's signature, and whether statuses from that user should be ignored.

### !list-accounts
Usage: !list-accounts [user (optional for accounts with 1 user)]

Lists all the accounts that can attribute posts to this user, indicating which, if any, is the primary account.

### !remove-user
Usage: !remove-user [user]

This will remove the association between the user and the account sending the command. This will stop the bot from attributing toots from the removed account to the user. Other accounts associated with the user will not be affected. The bot will prompt for the removal, and the user can procede by replying yes. If the account sending the command is the only account associated with the given user, an error will direct the user to use !delete-user instead.

### !delete-user
Usage: !delete-user [user]

This will permanently delete the given user by removing all associations between the user and its accounts. If the account issuing the command is the only account associated with the user, the bot will prompt for the deletion normally. If other accounts are associated with the given user as well, they will all be prompted for the deletion, and it will only procede after all accounts have replyed yes.

### !delete-account
Usage: !delete-account

This will permanently delete the account from the bot. The bot will prompt the user for the deletion, and the user can procede by replying yes. This command will also delete all users that only have an association with this account. This will specifically be warned about in the prompt.


## Anti-Harrassment

This bot only summarizes individual posts if the user has an account with the bot and they've unlocked their account. If your account is unlocked but someone is using this bot to harrass you, you can block them or their entire domain. An account or domain on your blocklist that mentions us will not cause us to analyze any status. Additionally, the blocked account will not be able to mention the blocker through commands that require another account's permission, like !add-account and !delete-user. 

Accounts and domains that repeatedly use this bot to harrass others will be blocked by this account as a whole. In addition to this, the bot domain blocks many domains already known for spam, bigotry, and other unwanted content.

### !block
Usage: !block [username] at [domain] [reason (optional)]

This command will add the given account to the user's blocklist.

### !domain-block

Usage: !domain-block [domain] [reason (optional)]

This commands will add the given domain to the user's blocklist.

### Alternate Usage:
If someone is spamming you with mentions through this bot, you can reply as a DM to either a toot of theirs, or a summary sent by the bot with a !block or !domain-block command.

### !blocklist
Usage: !blocklist [user (optional for accounts with 1 user)]

The bot will reply with all blocked accounts (they will not be mentioned) and their reason.

### !unblock
Usage: !unblock [username] at [domain]

Usage: !unblock [domain]

This command removes the given account or domain from your blocklist.

## Caveats
Until I find a good way for users to customize their keyboard, the bot assumes that all toots were typed on my keyboard, probably because all the lizards are at my house for a lizard party of some sort.

This bot does not store the contents of any statuses. However, it does store the IDs of toots as seen by botsin.space.
