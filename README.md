[![Build](https://github.com/VincentCordobes/prep/workflows/Build/badge.svg)](https://github.com/VincentCordobes/prep/actions?query=workflow%3ABuild)

<p align="center">
<img src="https://user-images.githubusercontent.com/7091110/75851923-1f2d9480-5deb-11ea-96d8-312eef590016.png" alt="prep" />
</p>
<p align="center">
  <strong>Prep is a command line utility to help learn, memorize things.</strong><br/>
  <strong>It is based on the spaced repetition learning technique.</strong>
</p>

<p align="center">
<img src="https://user-images.githubusercontent.com/7091110/87849761-6e341c00-c8eb-11ea-8067-eadbaf500362.png" alt="screenshot" />
</p>

## Why ?

We all love to learn new things. Whether it is a language, a song, a musical instrument, a specific topic etc..

The difficulty is not so much to learn those things per se. The difficulty is to memorize them over the long term, to master these things.
Too often, once we've learned something, we take it for granted, we don't bother to review it, and we forget it. **Too bad, we knew it once!** 

The key to improve long-term memorization is to **distribute practice session over time**. That technique is called _spaced repetition_. **prep** is there to help you manage everything you want to learn. 


The principle is simple. We have cards put into ~~platic~~ boxes.
A card is an item you want to learn/practice. It could be anything—from a plain plain text file to a video, a PDF, a song and so one. 

Boxes are associated with a time interval—duration between each practice session. 
When we successfully review a card, it graduates to the next box and will be shown less often. Otherwise it goes down to the previous one.
The first box is well suited for newly added cards or cards that are hard to grasp while the last box contains cards we already master.


Decks groups related card together. Let's say we are learning the drums and the capitals of the countries of the world. When starting a practice session, we don't want to mix them. It's a good idea to create a Deck for each of two.


It is highly inspired by the _Leitner system_. Unlike SuperMemo—a popular spaced repetition software—it is **not** intended to provide an optimal interval between sessions. That is to say, it is up to the user to setup the frequencies of the practice sessions.



## Installation


```sh
curl -sSL https://raw.githubusercontent.com/VincentCordobes/prep/master/scripts/install.sh | sudo sh
```

_Note: Linux and macos are supported. See the releases section._



## References
- https://apps.ankiweb.net/docs/manual.html
- https://www.supermemo.com/en/archives1990-2015/articles/decalog
- https://en.wikipedia.org/wiki/Spaced_repetition
