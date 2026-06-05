<picture>
  <img src="./assets/goblinlogogy.png" alt="Goblin" width="500">
</picture>

# Goblin Programming Language

Goblin is a scripting language built for simulations, automation, tooling, games, static site generation, and rapid application development.

Goblin focuses on readability, discoverability, and reducing unnecessary programming vocabulary. Instead of requiring programmers to memorize dozens of unrelated APIs and language quirks, Goblin provides a consistent set of concepts and verbs that work across the language.

## Features

* Simple, readable syntax
* Built-in collections, maps, strings, money, dates, and durations
* Randomization and dice systems
* Classes, objects, overlays, links, and transitions
* Simulation-focused tooling
* Static site tooling through Sheriff
* Package system through GLAMs
* Cross-platform CLI

## Installation

Download the latest release from GitHub.

Current release assets:

* Windows x64
* Linux x64

After downloading:

```text
goblin --version
```

Run a script:

```text
goblin run hello.gbln
```

Start the REPL:

```text
goblin
```

## Example

```gbln
name | "Goblin"

:say("Welcome to the Horde, {name}.")
```

Output:

```text
Welcome to the Horde, Goblin.
```

## Current Status

Goblin is actively developed and used as the foundation for several Goblin ecosystem projects, including:

* Sheriff (static site generator)
* Sheriff Cloud
* Spur
* Holster
* Future GLAM packages

Current release:

```text
v0.46.2
```

Goblin is stable enough for experimentation and personal projects, but the language and standard library continue to evolve.

## Documentation

Documentation is available at:

https://goblinlang.org

## License

MIT License
