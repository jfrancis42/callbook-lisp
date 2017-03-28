# callbook

Callbook is a library for pulling ham radio license info for hams from either the callbook.info API or the HamQTH.com API and returning an object with information about the operator and his QTH. callbook.info requires no pre-registration, but contains data for US hams only. HamQTH.como requires you to create a free account on their web site before you can fetch data via the API.

Using the API is very simple. First, an example of fetching info from callbook.info. This requires nothing more than the callsign to be looked up:

````
CL-USER> (describe (callbook:callbook-lookup "n0clu"))
#<CALLBOOK::HAM {10109ADE13}>
  [standard-object]

Slots with :INSTANCE allocation:
  SERIAL-NUMBER    = 0
  CREATION-TIME    = 1490668490
  CREATION-SOURCE  = 0
  NAME             = "DAVID M FRIEDMAN"
  DESCRIPTION      = ""
  LAT              = 33.06799
  LON              = -96.90841
  DATUM            = "WGS84"
  CALL             = "N0CLU"
  NAME             = "DAVID M FRIEDMAN"
  LIC-CLASS        = "TECHNICIAN"
  GRID             = "EM13nb"
  LAT              = 33.06799
  LON              = -96.90841
  EMAIL            = NIL
  STREET           = "1112 ANNALEA COVE DRIVE"
  CITY             = "LEWISVILLE, TX 75056"
  COUNTRY          = "United States"
; No value
CL-USER>
````

Using the HamQTH lookup is equally simple, but also requires a login and password:

````
CL-USER> (describe (callbook:hamqth-lookup "n0bdy" "secret" "n0clu"))
#<CALLBOOK::HAM {1010BD4B63}>
  [standard-object]

Slots with :INSTANCE allocation:
  SERIAL-NUMBER    = 1
  CREATION-TIME    = 1490668532
  CREATION-SOURCE  = 0
  NAME             = "David M Friedman"
  DESCRIPTION      = ""
  LAT              = 31.106
  LON              = -97.6475
  DATUM            = "WGS84"
  CALL             = "N0CLU"
  NAME             = "David M Friedman"
  LIC-CLASS        = NIL
  GRID             = "EM13NC"
  LAT              = 31.106
  LON              = -97.6475
  EMAIL            = NIL
  STREET           = "1112 Annalea Cove Drive"
  CITY             = "Lewisville TX 75056"
  COUNTRY          = "United States"
; No value
CL-USER>
````

Not all information is available from both sites, and object slots for which values are not available are set to nil. Note that the ham object is subclassed from the 2d-point object in the aviation formulary (available here: https://github.com/jfrancis42/aviation-formulary-lisp) so that the full suite of distance/direction functions in the aviation formulary are available for computing distance and bearing to the other ham from your QTH. For example, to let's assume that you're N0DUH. Calculate the distance from your QTH (which presumably you already know, so you supply the lat and long) to the QTH of N0CLU (which is fetched from the web):

````
CL-USER> (af:rad-to-km (af:calc-distance (make-instance 'af:2d-point :lat 34.17072 :lon -118.33018 :name "n0duh") (callbook:callbook-lookup "n0clu")))
1982.3779929630218d0
````

About 1982 kilometers. Now calculate the bearing:

````
CL-USER> (af:rad-to-deg (af:calc-gc-bearing (make-instance 'af:2d-point :lat 34.17072 :lon -118.33018 :name "n0duh") (callbook:callbook-lookup "n0clu")))
87.51722565236864d0
CL-USER>
````

About 88 degrees. Easy peasy.
