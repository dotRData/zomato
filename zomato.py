import json
import os
import sys
import urllib2

headers  = {"Accept":"application/json", "user_key":"07b65c0fd9ac93e8c6bb905f468eaa14"}
res_id   = 16774318
url      = 'https://developers.zomato.com/api/v2.1/reviews?res_id='+str(res_id)+'"'

request  = urllib2.Request(url, headers=headers)
contents = json.loads(urllib2.urlopen(request).read())

for review in contents['user_reviews']:
    print review['review']['review_text']
    print ("*******" * 4)


