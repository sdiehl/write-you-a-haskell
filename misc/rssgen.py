import datetime
import PyRSS2Gen

jan = datetime.datetime(2015, 1, 1, 0, 0)
feb = datetime.datetime(2015, 2, 1, 0, 0)
mar = datetime.datetime(2015, 3, 1, 0, 0)
apr = datetime.datetime(2015, 4, 1, 0, 0)
may = datetime.datetime(2015, 5, 1, 0, 0)
jun = datetime.datetime(2015, 6, 1, 0, 0)
jul = datetime.datetime(2015, 7, 1, 0, 0)
aug = datetime.datetime(2015, 8, 1, 0, 0)

pages = [
   PyRSS2Gen.RSSItem(
     title = "Introduction",
     link = "http://dev.stephendiehl.com/fun/000_introduction.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/000_introduction.html"),
     pubDate = jan + datetime.timedelta(1) ),

   PyRSS2Gen.RSSItem(
     title = "Haskell Basics",
     link = "http://dev.stephendiehl.com/fun/001_basics.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/001_basics.html"),
     pubDate = jan + datetime.timedelta(2) ),

   PyRSS2Gen.RSSItem(
     title = "Parsing",
     link = "http://dev.stephendiehl.com/fun/002_parsers.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/002_parsers.html"),
     pubDate = jan + datetime.timedelta(3)),

   PyRSS2Gen.RSSItem(
     title = "Lambda Calculus",
     link = "http://dev.stephendiehl.com/fun/003_lambda_calculus.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/003_lambda_calculus.html"),
     pubDate = jan + datetime.timedelta(4)),

   PyRSS2Gen.RSSItem(
     title = "Type Systems",
     link = "http://dev.stephendiehl.com/fun/004_type_systems.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/004_type_systems.html"),
     pubDate = jan + datetime.timedelta(5)),

   PyRSS2Gen.RSSItem(
     title = "Evaluation",
     link = "http://dev.stephendiehl.com/fun/005_evaluation.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/005_evaluation.html"),
     pubDate = jan + datetime.timedelta(6)),

   PyRSS2Gen.RSSItem(
     title = "Hindley-Milner Inference",
     link = "http://dev.stephendiehl.com/fun/006_hindley_milner.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/006_hindley_milner.html"),
     pubDate = jan + datetime.timedelta(7)),

   PyRSS2Gen.RSSItem(
     title = "Design of ProtoHaskell",
     link = "http://dev.stephendiehl.com/fun/007_path.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/007_path.html"),
     pubDate = jan + datetime.timedelta(8)),

   PyRSS2Gen.RSSItem(
     title = "Extended Parser",
     link = "http://dev.stephendiehl.com/fun/008_extended_parser.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/008_extended_parser.html"),
     pubDate = datetime.datetime(2015, 1, 24, 14, 30, 28, 996866))
]

rss = PyRSS2Gen.RSS2(
    title = "Write You A Haskell",
    link = "http://dev.stephendiehl.com/",
    description = "Building a modern functional compiler from first principles.",
    lastBuildDate = datetime.datetime.now(),
    items = pages
    )


if __name__ == '__main__':
    print "Generating RSS Feeding: atom.xml"
    rss.write_xml(open("atom.xml", "w"))
