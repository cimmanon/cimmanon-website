![Cimmanon](https://rawgithub.com/cimmanon/cimmanon-website/master/static/images/cimmanon.svg)

This is the source code to [my portfolio website](http://cimmanon.org/).  It replaces my portfolio's hand-coded markup that's been limping along since 2002.  It's not just for web designs, though, it can accomodate coding or any other type of project I might want to do.  In addition to being able to write up a little blurb and upload screenshots, the ArchiveServe will serve up past web designs in all of their original glory.

I originally started rewriting/redesigning the site in February 2014.  Then I got busy with a new job and ended up never having time to work on it.  A few months later, I had a hard drive failure lost everything but a small HTML/CSS demo I had uploaded to my webserver.  I didn't think that what I had written so far was at a point where I wanted anyone to see it, so I kept it entirely local.  Lesson learned.

This version was started at the end of December 2015 and launched January 26, 2015.  About half of the design choices I made in the original version made it into this version.

# Setup

If you actually want to try running it, you'll need GHC installed.  You'll also need a few extra libraries I've worked on that aren't on hackage.

* [postgresql-simple-tuple](https://github.com/cimmanon/postgresql-simple-tuple)
* [snap-handlers](https://github.com/cimmanon/snap-handlers)
* [camellia-splices](https://github.com/cimmanon/camellia-splices)
* [digestive-functors](https://github.com/cimmanon/digestive-functors) (requires modifications from the `choice-default` and `clean-choice-values` branchs)
* [digestive-functors-heist-extras](https://github.com/cimmanon/digestive-functors-heist-extras)

I *think* this should install everything correctly:


```
cabal sandbox init
cabal install snap
cabal sandbox add-source path/to/postgresql-simple-tuple
cabal sandbox add-source path/to/snap-handlers
cabal sandbox add-source path/to/camellia-splices
cabal sandbox add-source path/to/digestive-functors/digestive-functors
cabal sandbox add-source path/to/digestive-functors/digestive-functors-heist
cabal install
cabal run
```

You'll also need a PostgreSQL database.  Credentials are in `snaplets/postgreql-simple/` (this file should auto generate when you run the application, I think).

```
psql -f setup/setup.sql
```
