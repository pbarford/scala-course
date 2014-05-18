import objsets._

val ts1 = new NonEmpty(new Tweet("paul", "hi", 1), new Empty, new Empty)
val ts2 = ts1.incl(new Tweet("anita", "bonjour", 2)).incl(new Tweet("anita", "abc", 4))
val ts3 = ts2.incl(new Tweet("alexandra", "ta ta", 6)).incl(new Tweet("anita", "aaaaaa", 3))
ts3.foreach(f => println(f.text))





val ts4 = ts3.filter(t => t.user == "anita")
ts4.foreach(t => println(t.text))



ts4.union(ts1).foreach(t => println(t.text))




println(ts3.mostRetweeted.text)

ts3.descendingByRetweet.foreach(t => println(t.text))





val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
lazy val googleTweets: TweetSet = google.map(t => TweetReader.allTweets.filter(tw => tw.text.contains(t))).reduceLeft(_.union(_))
googleTweets.foreach(t => println(t.text))






































