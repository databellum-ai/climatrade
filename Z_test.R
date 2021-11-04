

testok <- tryCatch(read_html("https://spotifycharts.com/regional/us/daily/2021-09-25"), error = function(e){NA})
testko <- tryCatch(read_html("https://spotifycharts.com/regional/cy/daily/2021-09-25"), error = function(e){NA})

testok
testko









