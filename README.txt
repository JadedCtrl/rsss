===============================================================================
RSSS                                                        An RSS/Atom parser
===============================================================================

Reading Syndicated Stuff Sagely is made to make your life (hopefully) a bit
easier when you've got to deal with Atom/RSS feed XML, especially when you need
to handle *both* types of feed.

It generalizes all types of feed (Atom, RSS 2.0/1.0) into a single RSSS:FEED
object, with subsequent RSSS:ENTRY objects inside for <entry>/<item>s within
the feed.


————————————————————————————————————————
USAGE
————————————————————————————————————————
You can turn a feed's XML (string) into an RSSS:FEED object with #'rsss:parse.

Then, you can read it by means of it's slots.

Slots of both FEEDs and ENTRYs:
* name
* date
* desc
* uri

Slots exclusively for FEEDs:
* entries

Slots exclusively for ENTRYs:
* author
* media
* text

Each slot has an accessor in the :rsss package, like #'rsss:media, etc.
Good luck!


————————————————————————————————————————
BORING STUFF
————————————————————————————————————————
License is in COPYING.txt (GNU GPLv3)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/rsss.git
