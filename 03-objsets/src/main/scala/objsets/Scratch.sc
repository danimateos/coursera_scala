import objsets._


val setOne = (new Empty).incl(
  new Tweet("c", "cccc", 10)).incl(
  new Tweet("a", "aaaa", 20)).incl(
  new Tweet("d", "ddd", 0))


val setTwo = (new Empty).incl(
  new Tweet("c", "cccc", 0)).incl(
  new Tweet("d", "dddd", 0)).incl(
  new Tweet("b", "bbbb", 5)).incl(
  new Tweet("e", "eeeee", 0))


setOne.filter(_.retweets > 10)
setTwo
