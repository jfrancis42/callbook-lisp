# callbook

Callbook is a library for pulling ham radio license info for US hams from the callbook.info API. It's ridiculously simple, but packages all of the libraries, etc. in one easy-to-reference place. There's only one call, and it returns a data structure you can use assoc to access. Example:

````
CL-USER> (defvar n0clu (callbook:lookup-call "n0clu"))
N0CLU
CL-USER> n0clu
((:STATUS . "VALID") (:TYPE . "PERSON")
 (:CURRENT (:CALLSIGN . "N0CLU") (:OPER-CLASS . "TECHNICIAN"))
 (:PREVIOUS (:CALLSIGN . "KG6JJV") (:OPER-CLASS . "TECHNICIAN PLUS"))
 (:TRUSTEE (:CALLSIGN . "") (:NAME . "")) (:NAME . "DAVID M FRIEDMAN")
 (:ADDRESS (:LINE-1 . "1112 ANNALEA COVE DRIVE")
  (:LINE-2 . "LEWISVILLE, TX 75056") (:ATTN . ""))
 (:LOCATION (:LATITUDE . "33.06799") (:LONGITUDE . "-96.90841")
  (:GRIDSQUARE . "EM13nb"))
 (:OTHER-INFO (:GRANT-DATE . "01/08/2013") (:EXPIRY-DATE . "04/01/2023")
  (:LAST-ACTION-DATE . "01/08/2013") (:FRN . "0002031573")
  (:ULS-URL
   . "http://wireless2.fcc.gov/UlsApp/UlsSearch/license.jsp?licKey=2499771")))
CL-USER> (assoc :address n0clu)
(:ADDRESS (:LINE-1 . "1112 ANNALEA COVE DRIVE")
 (:LINE-2 . "LEWISVILLE, TX 75056") (:ATTN . ""))
CL-USER>
````

That's it. Pretty trivial. Invalid (or non-US) callsigns return this:

````
CL-USER> (callbook:lookup-call "co2kk")
((:STATUS . "INVALID"))
CL-USER> 
````

Yes, it's annoying that it doesn't include non-US callsign data. If anyone has a pointer to a free API service that includes world callsign info, I'll happily update the library.
