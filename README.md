# Sxcrape

Sxcrape is a collection of tools for scraping the SXSW music show
schedule (known officially as the "Music Showcase").

The original plan for Sxcrape was to present the schedule with a REST
interface. However, its design goals have been simplified; now, it
parses the music schedule and presents it as a large JSON object. The
REST functionality may be implemented at a later date.

Currently, it consists of two tools:

* `sxcrape`, which parses the schedule from the official [SXSW
Schedule website](http://schedule.sxsw.com/) and creates, for each
event, a file containing the JSON representation of the event details
-- artist name, venue, showtime, etc.

* `sxred`, which imports event pages as JSON into a Redis database,
and exports the entire database as one large JSON object

## Caveats

Sxcrape is very year-dependent. This particular version is only
capable of parsing the 2012 music event schedule, and not any earlier
or later years, nor any other type of SXSW event.

## Running

### sxcrape

<pre>
% sxcrape --help
sxcrape 2012.9

sxcrape [COMMAND] ... [OPTIONS]
  Scrape the SXSW music schedule

Common flags:
  -? --help            Display help message
  -V --version         Print version information

sxcrape events [OPTIONS]
  Get music event URLs

  -d --day=DAY         Get a specific day (default is all days)

sxcrape dump [OPTIONS] URL
  Download music event HTML

sxcrape batchdump [OPTIONS] [-|URL ..]
  Download events from one or more URLs given on the command line, or via
  stdin; and write the HTML of each to a separate file

  -o --output-dir=DIR  output dump files in this directory
  -q --quiet           don't echo URLs on stdout

sxcrape parse [OPTIONS] URL|PATH
  Parse music event details into JSON, using a URL or the path to a file
  containing the event HTML

sxcrape batchparse [OPTIONS] [-|(URL|PATH ...)]
  Parse events from one or more URLs given on the command line, or via stdin,
  into JSON, and store each in a separate file

  -o --output-dir=DIR  output JSON files in this directory
  -q --quiet           don't echo URLs|PATHs on stdout
</pre>

### sxred

<pre>
% sxred --help
sxred 2012.9

sxred [COMMAND] ... [OPTIONS]
  Redis database for the SXSW music schedule

Common flags:
  -? --help     Display help message
  -V --version  Print version information

sxred batchimport [OPTIONS] [-|(URL|PATH ...)]
  Parse events from one or more URLs given on the command line, or via stdin,
  and import into Redis.

  -q --quiet    don't echo URLs|PATHs on stdout

sxred batchdump [OPTIONS]
  Dump entire Redis database into JSON format.
</pre>

## Per-event JSON representation

The scraper extracts the following information from each music event
page:

* Artist name, origin (e.g., San Francisco, CA), and website.
* Venue name, address, and age restrictions.
* Performance date.
* Scheduled start and end times.
* The genre/category assigned to the artist by SXSW.
* An image of the artist.
* A link to one of the artist's songs.
* A link to one of the artist's music videos.
* SXSW's recommended Twitter hashtags for the event.

The data are represented as name/value pairs in a JSON object, which
contains one object for the event, one for the artist, and one for the
venue. The event object contains the names of the artist and venue for
the purpose of establishing relationships in a database. Often, one or
more values are missing; these are represented in the JSON as null
values.

Here's an example of the JSON output from the `sxcrape` tool for the
[March 14, 2012 performance by Paper
Diamond](http://schedule.sxsw.com/2012/events/event_MS19763). The
first element in the list is the event details, the second is the
artist details, and the third is the venue details:

<pre>
[
  {
    "end" : "2012-03-15T05:50:00Z",
    "artist" : "Paper Diamond",
    "ages" : "21+",
    "start" : "2012-03-15T05:00:00Z",
    "venue" : "The Madison",
    "day" : "2012-03-14",
    "hashTags" : [
      "#sxsw",
      "#PaperDiamond"
    ],
    "url" : "http:\/\/schedule.sxsw.com\/2012\/events\/event_MS19763"
  },
  {
    "origin" : "Boulder, CO",
    "imgURL" : "http:\/\/img.sxsw.com\/2012\/bands\/2675.jpg",
    "videoURL" : null,
    "songURL" : null,
    "genre" : "Electronic",
    "name" : "Paper Diamond",
    "url" : "http:\/\/paper-diamond.com\/"
  },
  {
    "name" : "The Madison",
    "address" : "307-A W 5th St"
  }
]
</pre>

## License

Sxcrape is published under the [BSD
3-Clause](http://opensource.org/licenses/BSD-3-Clause) license.

## Contact

For comments or questions, please [contact
us](mailto:src@quixoftic.com).
