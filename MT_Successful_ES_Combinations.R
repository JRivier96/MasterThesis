############################################################################
############################################################################
###                                                                      ###
###                           Years                                      ###
###                                                                      ###
############################################################################
############################################################################

############################################1. 2020 - 1 shock
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2020 >= quantile(unique(X$Score2020), .66) & 
            X$when <= "2020-09-21" & X$when >= "2020-04-22",]

Middle <- X[X$Score2020 > quantile(unique(X$Score2020), .33) & 
              X$Score2020 < quantile(unique(X$Score2020), .66) & 
              X$when <= "2020-09-21" & X$when >= "2020-04-22",]

Bad <- X[X$Score2020 <= quantile(unique(X$Score2020), .33) & X$Score2020 > 0 & 
           X$when <= "2020-09-21" & X$when >= "2020-04-22",]

############################################2. 2019 - 1 shock
X <- Event[Event$Sector == "Energy",]
Good <- X[X$Score2019 >= quantile(unique(X$Score2019), .66) & 
            X$when <= "2019-12-31" & X$when >= "2019-01-01",]

Middle <- X[X$Score2019 > quantile(unique(X$Score2019), .33) & 
              X$Score2019 < quantile(unique(X$Score2019), .66) & 
              X$when <= "2019-12-31" & X$when >= "2019-01-01",]

Bad <- X[X$Score2019 <= quantile(unique(X$Score2019), .33) & X$Score2019 > 0 & 
           X$when <= "2019-12-31" & X$when >= "2019-01-01",]

############################################3. 2018 - 1 shock
X <- Event[Event$Sector == "Energy",]
Good <- X[X$Score2018 >= quantile(unique(X$Score2018), .66) & 
            X$when <= "2018-12-31" & X$when >= "2018-01-01",]

Middle <- X[X$Score2018 > quantile(unique(X$Score2018), .33) & 
              X$Score2018 < quantile(unique(X$Score2018), .66) & 
              X$when <= "2018-12-31" & X$when >= "2018-01-01",]

Bad <- X[X$Score2018 <= quantile(unique(X$Score2018), .33) & X$Score2018 > 0 & 
           X$when <= "2018-12-31" & X$when >= "2018-01-01",]

############################################4. 2016 - 1 shock
X <- Event[Event$Sector == "Energy",]
Good <- X[X$Score2016 >= quantile(unique(X$Score2016), .66) & 
            X$when <= "2016-12-31" & X$when >= "2016-01-04",]

Middle <- X[X$Score2016 > quantile(unique(X$Score2016), .33) & 
              X$Score2016 < quantile(unique(X$Score2016), .66) & 
              X$when <= "2016-12-31" & X$when >= "2016-01-04",]

Bad <- X[X$Score2016 <= quantile(unique(X$Score2016), .33) & X$Score2016 > 0 & 
           X$when <= "2016-12-31" & X$when >= "2016-01-04",]

###########################################5. 2015 - 3 shocks (01-09-2015 removed)

#1 2015-07-06
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2015 >= quantile(unique(X$Score2015), .66) & 
            X$when <= "2015-07-06" & X$when >= "2015-01-02",]

Middle <- X[X$Score2015 > quantile(unique(X$Score2015), .33) & 
              X$Score2015 < quantile(unique(X$Score2015), .66) & 
              X$when <= "2015-07-06" & X$when >= "2015-01-02",]

Bad <- X[X$Score2015 <= quantile(unique(X$Score2015), .33) & X$Score2015 > 0 & 
           X$when <= "2015-07-06" & X$when >= "2015-01-02",]

#2 2015-08-24
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2015 >= quantile(unique(X$Score2015), .66) & 
            X$when <= "2015-08-24" & X$when >= "2015-07-07",]

Middle <- X[X$Score2015 > quantile(unique(X$Score2015), .33) & 
              X$Score2015 < quantile(unique(X$Score2015), .66) & 
              X$when <= "2015-08-24" & X$when >= "2015-07-07",]

Bad <- X[X$Score2015 <= quantile(unique(X$Score2015), .33) & X$Score2015 > 0 & 
           X$when <= "2015-08-24" & X$when >= "2015-07-07",]

#3 2015-12-07
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2015 >= quantile(unique(X$Score2015), .66) & 
            X$when <= "2015-12-07" & X$when >= "2015-09-02",]

Middle <- X[X$Score2015 > quantile(unique(X$Score2015), .33) & 
              X$Score2015 < quantile(unique(X$Score2015), .66) & 
              X$when <= "2015-12-07" & X$when >= "2015-09-02",]

Bad <- X[X$Score2015 <= quantile(unique(X$Score2015), .33) & X$Score2015 > 0 & 
           X$when <= "2015-12-07" & X$when >= "2015-09-02",]

############################################6. 2014 - 1 shock
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2014 >= quantile(unique(X$Score2014), .66) & 
            X$when <= "2014-12-31" & X$when >= "2014-01-02",]

Middle <- X[X$Score2014 > quantile(unique(X$Score2014), .33) & 
              X$Score2014 < quantile(unique(X$Score2014), .66) & 
              X$when <= "2014-12-31" & X$when >= "2014-01-02",]

Bad <- X[X$Score2014 <= quantile(unique(X$Score2014), .33) & X$Score2014 > 0 & 
           X$when <= "2014-12-31" & X$when >= "2014-01-02",]

############################################7. 2013 - 2 shocks
#1 2013-04-15

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2013 >= quantile(unique(X$Score2013), .66) & 
            X$when <= "2013-04-15" & X$when >= "2013-01-02",]

Middle <- X[X$Score2013 >= quantile(unique(X$Score2013), .33) & 
              X$Score2013 < quantile(unique(X$Score2013), .66) & 
              X$when <= "2013-04-15" & X$when >= "2013-01-02",]

Bad <- X[X$Score2013 < quantile(unique(X$Score2013), .33) & X$Score2013 > 0 & 
           X$when <= "2013-04-15" & X$when >= "2013-01-02",]

#2 2013-06-20
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2013 >= quantile(unique(X$Score2013), .66) & 
            X$when <= "2013-06-20" & X$when >= "2013-04-16",]

Middle <- X[X$Score2013 >= quantile(unique(X$Score2013), .33) & 
              X$Score2013 < quantile(unique(X$Score2013), .66) & 
              X$when <= "2013-06-20" & X$when >= "2013-04-16",]

Bad <- X[X$Score2013 < quantile(unique(X$Score2013), .33) & X$Score2013 > 0 & 
           X$when <= "2013-06-20" & X$when >= "2013-04-16",]

############################################8. 2012 - 1 shock
X <- Event[Event$Sector == "Energy",]
Good <- X[X$Score2012 >= quantile(unique(X$Score2012), .66) & 
            X$when <= "2012-12-31" & X$when >= "2012-01-03",]

Middle <- X[X$Score2012 > quantile(unique(X$Score2012), .33) & 
              X$Score2012 < quantile(unique(X$Score2012), .66) & 
              X$when <= "2012-12-31" & X$when >= "2012-01-03",]

Bad <- X[X$Score2012 <= quantile(unique(X$Score2012), .33) & X$Score2012 > 0 & 
           X$when <= "2012-12-31" & X$when >= "2012-01-03",]

############################################8. 2011 - 5 shocks
#1 2011-03-15

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2011 >= quantile(unique(X$Score2011), .66) & 
            X$when <= "2011-03-15" & X$when >= "2011-01-03",]

Middle <- X[X$Score2011 >= quantile(unique(X$Score2011), .33) & 
              X$Score2011 < quantile(unique(X$Score2011), .66) & 
              X$when <= "2011-03-15" & X$when >= "2011-01-03",]

Bad <- X[X$Score2011 < quantile(unique(X$Score2011), .33) & X$Score2011 > 0 & 
           X$when <= "2011-03-15" & X$when >= "2011-01-03",]

#2 2011-05-05

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2011 >= quantile(unique(X$Score2011), .66) & 
            X$when <= "2011-05-05" & X$when >= "2011-03-16",]

Middle <- X[X$Score2011 >= quantile(unique(X$Score2011), .33) & 
              X$Score2011 < quantile(unique(X$Score2011), .66) & 
              X$when <= "2011-05-05" & X$when >= "2011-03-16",]

Bad <- X[X$Score2011 < quantile(unique(X$Score2011), .33) & X$Score2011 > 0 & 
           X$when <= "2011-05-05" & X$when >= "2011-03-16",]

#3 2011-08-04

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2011 >= quantile(unique(X$Score2011), .66) & 
            X$when <= "2011-08-04" & X$when >= "2011-05-12",]

Middle <- X[X$Score2011 >= quantile(unique(X$Score2011), .33) & 
              X$Score2011 < quantile(unique(X$Score2011), .66) & 
              X$when <= "2011-08-04" & X$when >= "2011-05-12",]

Bad <- X[X$Score2011 < quantile(unique(X$Score2011), .33) & X$Score2011 > 0 & 
           X$when <= "2011-08-04" & X$when >= "2011-05-12",]

#4 2011-09-22

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2011 >= quantile(unique(X$Score2011), .66) & 
            X$when <= "2011-09-22" & X$when >= "2011-08-05",]

Middle <- X[X$Score2011 >= quantile(unique(X$Score2011), .33) & 
              X$Score2011 < quantile(unique(X$Score2011), .66) & 
              X$when <= "2011-09-22" & X$when >= "2011-08-05",]

Bad <- X[X$Score2011 < quantile(unique(X$Score2011), .33) & X$Score2011 > 0 & 
           X$when <= "2011-09-22" & X$when >= "2011-08-05",]

#5 2011-11-17

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2011 >= quantile(unique(X$Score2011), .66) & 
            X$when <= "2011-11-17" & X$when >= "2011-10-03",]

Middle <- X[X$Score2011 >= quantile(unique(X$Score2011), .33) & 
              X$Score2011 < quantile(unique(X$Score2011), .66) & 
              X$when <= "2011-11-17" & X$when >= "2011-10-03",]

Bad <- X[X$Score2011 < quantile(unique(X$Score2011), .33) & X$Score2011 > 0 & 
           X$when <= "2011-11-17" & X$when >= "2011-10-03",]


############################################8. 2010 - 2 shocks

#1 2010-05-14

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2010 >= quantile(unique(X$Score2010), .66) & 
            X$when <= "2010-05-14" & X$when >= "2010-01-04",]

Middle <- X[X$Score2010 >= quantile(unique(X$Score2010), .33) & 
              X$Score2010 < quantile(unique(X$Score2010), .66) & 
              X$when <= "2010-05-14" & X$when >= "2010-01-04",]

Bad <- X[X$Score2010 < quantile(unique(X$Score2010), .33) & X$Score2010 > 0 & 
           X$when <= "2010-05-14" & X$when >= "2010-01-04",]

#2 2010-11-12

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2010 >= quantile(unique(X$Score2010), .66) & 
            X$when <= "2010-11-12" & X$when >= "2010-06-30",]

Middle <- X[X$Score2010 >= quantile(unique(X$Score2010), .33) & 
              X$Score2010 < quantile(unique(X$Score2010), .66) & 
              X$when <= "2010-11-12" & X$when >= "2010-06-30",]

Bad <- X[X$Score2010 < quantile(unique(X$Score2010), .33) & X$Score2010 > 0 & 
           X$when <= "2010-11-12" & X$when >= "2010-06-30",]


############################################9. 2009 - 1 shock

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2009 >= quantile(unique(X$Score2009), .66) & 
            X$when <= "2009-10-28" & X$when >= "2009-08-17",]

Middle <- X[X$Score2009 >= quantile(unique(X$Score2009), .33) & 
              X$Score2009 < quantile(unique(X$Score2009), .66) & 
              X$when <= "2009-10-28" & X$when >= "2009-08-17",]

Bad <- X[X$Score2009 < quantile(unique(X$Score2009), .33) & X$Score2009 > 0 & 
           X$when <= "2009-10-28" & X$when >= "2009-08-17",]

############################################10. 2008 - 2 shocks

#1 2008-03-17

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2008 >= quantile(unique(X$Score2008), .66) & 
            X$when <= "2008-03-17" & X$when >= "2008-01-02",]

Middle <- X[X$Score2008 >= quantile(unique(X$Score2008), .33) & 
              X$Score2008 < quantile(unique(X$Score2008), .66) & 
              X$when <= "2008-03-17" & X$when >= "2008-01-02",]

Bad <- X[X$Score2008 < quantile(unique(X$Score2008), .33) & X$Score2008 > 0 & 
           X$when <= "2008-03-17" & X$when >= "2008-01-02",]

#2 2008-05-29

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2008 >= quantile(unique(X$Score2008), .66) & 
            X$when <= "2008-05-29" & X$when >= "2008-03-20",]

Middle <- X[X$Score2008 >= quantile(unique(X$Score2008), .33) & 
              X$Score2008 < quantile(unique(X$Score2008), .66) & 
              X$when <= "2008-05-29" & X$when >= "2008-03-20",]

Bad <- X[X$Score2008 < quantile(unique(X$Score2008), .33) & X$Score2008 > 0 & 
           X$when <= "2008-05-29" & X$when >= "2008-03-20",]

############################################11. 2007 - 2 shocks

#1 2007-01-03

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2007 >= quantile(unique(X$Score2007), .66) & 
            X$when <= "2007-01-03" & X$when >= "2006-11-13",]

Middle <- X[X$Score2007 >= quantile(unique(X$Score2007), .33) & 
              X$Score2007 < quantile(unique(X$Score2007), .66) & 
              X$when <= "2007-01-03" & X$when >= "2006-11-13",]

Bad <- X[X$Score2007 < quantile(unique(X$Score2007), .33) & X$Score2007 > 0 & 
           X$when <= "2007-01-03" & X$when >= "2006-11-13",]

#2 2007-08-16

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2007 >= quantile(unique(X$Score2007), .66) & 
            X$when <= "2007-08-16" & X$when >= "2007-01-04",]

Middle <- X[X$Score2007 >= quantile(unique(X$Score2007), .33) & 
              X$Score2007 < quantile(unique(X$Score2007), .66) & 
              X$when <= "2007-08-16" & X$when >= "2007-01-04",]

Bad <- X[X$Score2007 < quantile(unique(X$Score2007), .33) & X$Score2007 > 0 & 
           X$when <= "2007-08-16" & X$when >= "2007-01-04",]


############################################12. 2006 - 3 shocks

#1 2006-04-27

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2006 >= quantile(unique(X$Score2006), .66) & 
            X$when <= "2006-04-27" & X$when >= "2006-01-03",]

Middle <- X[X$Score2006 >= quantile(unique(X$Score2006), .33) & 
              X$Score2006 < quantile(unique(X$Score2006), .66) & 
              X$when <= "2006-04-27" & X$when >= "2006-01-03",]

Bad <- X[X$Score2006 < quantile(unique(X$Score2006), .33) & X$Score2006 > 0 & 
           X$when <= "2006-04-27" & X$when >= "2006-01-03",]

#2 2006-09-11
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2006 >= quantile(unique(X$Score2006), .66) & 
            X$when <= "2006-09-11" & X$when >= "2006-07-18",]

Middle <- X[X$Score2006 >= quantile(unique(X$Score2006), .33) & 
              X$Score2006 < quantile(unique(X$Score2006), .66) & 
              X$when <= "2006-09-11" & X$when >= "2006-07-18",]

Bad <- X[X$Score2006 < quantile(unique(X$Score2006), .33) & X$Score2006 > 0 & 
           X$when <= "2006-09-11" & X$when >= "2006-07-18",]

#3 2006-11-10
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2006 >= quantile(unique(X$Score2006), .66) & 
            X$when <= "2006-11-10" & X$when >= "2006-09-12",]

Middle <- X[X$Score2006 >= quantile(unique(X$Score2006), .33) & 
              X$Score2006 < quantile(unique(X$Score2006), .66) & 
              X$when <= "2006-11-10" & X$when >= "2006-09-12",]

Bad <- X[X$Score2006 < quantile(unique(X$Score2006), .33) & X$Score2006 > 0 & 
           X$when <= "2006-11-10" & X$when >= "2006-09-12",]

############################################12. 2005 - 1 shocks

X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2005 >= quantile(unique(X$Score2005), .66) & 
            X$when <= "2005-12-31" & X$when >= "2005-01-01",]

Middle <- X[X$Score2005 > quantile(unique(X$Score2005), .33) & 
              X$Score2005 < quantile(unique(X$Score2005), .66) & 
              X$when <= "2005-12-31" & X$when >= "2005-01-01",]

Bad <- X[X$Score2005 <= quantile(unique(X$Score2005), .33) & X$Score2005 > 0 & 
           X$when <= "2005-12-31" & X$when >= "2005-01-01",]

############################################12. 2004 - 1 shocks
#1 2004-01-15
X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2004 >= quantile(unique(X$Score2004), .66) & 
            X$when <= "2004-01-15" & X$when >= "2003-03-14",]

Middle <- X[X$Score2004 >= quantile(unique(X$Score2004), .33) & 
              X$Score2004 < quantile(unique(X$Score2004), .66) & 
              X$when <= "2004-01-15" & X$when >= "2003-03-14",]

Bad <- X[X$Score2004 < quantile(unique(X$Score2004), .33) & X$Score2004 > 0 & 
           X$when <= "2004-01-15" & X$when >= "2003-03-14",]

#2 2004-04-21
X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2004 >= quantile(unique(X$Score2004), .66) & 
            X$when <= "2004-04-21" & X$when >= "2004-01-16",]

Middle <- X[X$Score2004 >= quantile(unique(X$Score2004), .33) & 
              X$Score2004 < quantile(unique(X$Score2004), .66) & 
              X$when <= "2004-04-21" & X$when >= "2004-01-16",]

Bad <- X[X$Score2004 < quantile(unique(X$Score2004), .33) & X$Score2004 > 0 & 
           X$when <= "2004-04-21" & X$when >= "2004-01-16",]

#3 2004-12-01 /double shock 
X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2004 >= quantile(unique(X$Score2004), .66) & 
            X$when <= "2004-12-01" & X$when >= "2004-06-03",]

Middle <- X[X$Score2004 >= quantile(unique(X$Score2004), .33) & 
              X$Score2004 < quantile(unique(X$Score2004), .66) & 
              X$when <= "2004-12-01" & X$when >= "2004-06-03",]

Bad <- X[X$Score2004 < quantile(unique(X$Score2004), .33) & X$Score2004 > 0 & 
           X$when <= "2004-12-01" & X$when >= "2004-06-03",]

############################################14. 2003 - 1 shock

X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2003 >= quantile(unique(X$Score2003), .66) & 
            X$when <= "2003-02-25" & X$when >= "2003-01-02",]

Middle <- X[X$Score2003 > quantile(unique(X$Score2003), .33) & 
              X$Score2003 < quantile(unique(X$Score2003), .66) & 
              X$when <= "2003-02-25" & X$when >= "2003-01-02",]

Bad <- X[X$Score2003 <= quantile(unique(X$Score2003), .33) & X$Score2003 > 0 & 
           X$when <= "2003-02-25" & X$when >= "2003-01-02",]

############################################12. 2002 - 2 shocks
#1 2002-04-14
X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2002 >= quantile(unique(X$Score2002), .66) & 
            X$when <= "2002-04-14" & X$when >= "2002-01-04",]

Middle <- X[X$Score2002 >= quantile(unique(X$Score2002), .33) & 
              X$Score2002 < quantile(unique(X$Score2002), .66) & 
              X$when <= "2002-04-14" & X$when >= "2002-01-04",]

Bad <- X[X$Score2002 < quantile(unique(X$Score2002), .33) & X$Score2002 > 0 & 
           X$when <= "2002-04-14" & X$when >= "2002-01-04",]

#2 2002-10-03
X <- Event[Event$Sector == "Utilities",]

Good <- X[X$Score2002 >= quantile(unique(X$Score2002), .66) & 
            X$when <= "2002-10-03" & X$when >= "2002-04-05",]

Middle <- X[X$Score2002 >= quantile(unique(X$Score2002), .33) & 
              X$Score2002 < quantile(unique(X$Score2002), .66) & 
              X$when <= "2002-10-03" & X$when >= "2002-04-05",]

Bad <- X[X$Score2002 < quantile(unique(X$Score2002), .33) & X$Score2002 > 0 & 
           X$when <= "2002-10-03" & X$when >= "2002-04-05",]
