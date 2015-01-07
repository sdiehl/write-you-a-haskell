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
     link = "http://dev.stephendiehl.com/fun/introduction.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/introduction.html"),
     pubDate = jan + datetime.timedelta(1) ),

   PyRSS2Gen.RSSItem(
     title = "Haskell Basics",
     link = "http://dev.stephendiehl.com/fun/basics.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/basics.html"),
     pubDate = jan + datetime.timedelta(2) ),

   PyRSS2Gen.RSSItem(
     title = "Parsing",
     link = "http://dev.stephendiehl.com/fun/parsers.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/parsers.html"),
     pubDate = jan + datetime.timedelta(3)),

   PyRSS2Gen.RSSItem(
     title = "Lambda Calculus",
     link = "http://dev.stephendiehl.com/fun/lambda_calculus.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/lambda_calculus.html"),
     pubDate = jan + datetime.timedelta(4)),

   PyRSS2Gen.RSSItem(
     title = "Type Systems",
     link = "http://dev.stephendiehl.com/fun/type_systems.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/type_systems.html"),
     pubDate = jan + datetime.timedelta(5)),

   PyRSS2Gen.RSSItem(
     title = "Evaluation",
     link = "http://dev.stephendiehl.com/fun/evaluation.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/evaluation.html"),
     pubDate = jan + datetime.timedelta(6)),

   PyRSS2Gen.RSSItem(
     title = "Hindley-Milner Inference",
     link = "http://dev.stephendiehl.com/fun/hindley_milner.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/hindley_milner.html"),
     pubDate = jan + datetime.timedelta(7)),

   PyRSS2Gen.RSSItem(
     title = "Design of ProtoHaskell",
     link = "http://dev.stephendiehl.com/fun/path.html",
     description = "",
     guid = PyRSS2Gen.Guid("http://dev.stephendiehl.com/fun/path.html"),
     pubDate = jan + datetime.timedelta(8)),
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
