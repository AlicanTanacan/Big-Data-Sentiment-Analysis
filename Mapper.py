#!/usr/bin/python
import sys
import re

def main(argv):

	# Define regex patterns
	#httpNewPage = re.compile(r"^http://.*?(\s|\n)")
	httpNewPage = re.compile(r"^WARC-Target-URI: (https?://.*?(\s|\n))")

	iphoneRe = re.compile(r" iphone.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	samsungRe = re.compile(r" samsung.*?galaxy.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	sonyRe = re.compile(r" sony.*?xperia.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	nokiaRe = re.compile(r" nokia.*?lumina.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	htcRe = re.compile(r" htc.*?phone.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	iosRe = re.compile(r"\sios.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)
	googleRe = re.compile(r" google.*?android.*?(review|critique|look\sat|in\sdepth|analysis|evaluat|assess)", re.IGNORECASE)

	# For Sentiment

	# Create regex string variables
	separator = ".*?"
	rePositive = "(awesome|beneficial|better|breatkthrough|brilliant|collaborat|delight|dependab|easy|effective|efficient|enhance|enjoy|excellent|exceptional|good|great|surpass|happy|highest|ideal|impress|improved|incredible|innovative|nice|outperform|outstanding|perfect|pleased|positive|reliable|smooth|stable|superior|unmatched|unparalleled|unsurpassed|valuable|versatile)"
	reNegative = "(aggravat|annoy|aweful|bad|broke|complicat|confus|corrupt|cumbersome|defect|deficien|difficult|disappoint|dispprov|drawback|erratic|error|fatal|flaw|frustrat|inadequate|inconsistent|incorrect|inefficient|inferior|insecure|insufficient|malfunction|mistak|overload|problem|slow|terrible|trouble|ugly|unacceptab|unreliable|unstable|vulnerabl|weak)"
	reUncertain = "(ambiguous|anomaly|anticipate|appear|arbitrary|confusing|deviat|differ|fluctuat|hidden|imprecise|indefinite|instabilit|might|occasionally|perhaps|possibl|predict|random|reassess|reconsider|rumors|seldom|sometimes|conditional|sporadic|tentative|uncertain|undecided|unexpected|unfamiliar|unforseen|unknown|unplanned|unpredictable|unprov|unusual|variable|varies|vary)"
	reCamera = "(camera|isight)"
	reDisplay = "(screen|display)"
	rePerformance = "(performance|storage|speed|processor)"
	reOSPerformance = "(performance|speed)"

	camPosRe = re.compile(r"(" + reCamera + separator + rePositive + ")|(" + rePositive + separator + reCamera + ")")
	camNegRe = re.compile(r"(" + reCamera + separator + reNegative + ")|(" + reNegative + separator + reCamera + ")")
	camUncRe = re.compile(r"(" + reCamera + separator + reUncertain + ")|(" + reUncertain + separator + reCamera + ")")

	disPosRe = re.compile(r"(" + reDisplay + separator + rePositive + ")|(" + rePositive + separator + reDisplay + ")")
	disNegRe = re.compile(r"(" + reDisplay + separator + reNegative + ")|(" + reNegative + separator + reDisplay + ")")
	disUncRe = re.compile(r"(" + reDisplay + separator + reUncertain + ")|(" + reUncertain + separator + reDisplay + ")")

	perPosRe = re.compile(r"(" + rePerformance + separator + rePositive + ")|(" + rePositive + separator + rePerformance + ")")
	perNegRe = re.compile(r"(" + rePerformance + separator + reNegative + ")|(" + reNegative + separator + rePerformance + ")")
	perUncRe = re.compile(r"(" + rePerformance + separator + reUncertain + ")|(" + reUncertain + separator + rePerformance + ")")

	OSPerPosRe = re.compile(r"(" + reOSPerformance + separator + rePositive + ")|(" + rePositive + separator + reOSPerformance + ")")
	OSPerNegRe = re.compile(r"(" + reOSPerformance + separator + reNegative + ")|(" + reNegative + separator + reOSPerformance + ")")
	OSPerUncRe = re.compile(r"(" + reOSPerformance + separator + reUncertain + ")|(" + reUncertain + separator + reOSPerformance + ")")

	# Initialize the current url and pageBody variables
	currentUrl = ""
	pageBody = ""
	brokenLineCounter = 0

	# For each line in the stream
	while True:
		try:
			line = sys.stdin.readline()
		except EOFError:
			print brokenLineCounter
			break
		except Exception as e:
			print >>sys.stderr, "error({0})".format(e)
			continue
		if not line:
			break

		# Check to see if the current line is a page header
		pageHeaderMatch = httpNewPage.search(line)

		# If it's a page header
		if(pageHeaderMatch):
			#print('Its a header match')

			iphoneCount = 0
			samsungCount = 0
			sonyCount = 0
			nokiaCount = 0
			htcCount = 0
			iosCount = 0
			googleCount = 0

			# Look for product matches
			for hit in iphoneRe.findall(pageBody, re.I):
				print currentUrl + ",1,0,0,0,0,0,0" + \
					blankSection(9) + \
					blankOS(3)
				iphoneCount += 1

			for hit in samsungRe.findall(pageBody, re.I):
				print currentUrl + ",0,1,0,0,0,0,0" + \
					blankSection(9) + \
					blankOS(3)
				samsungCount += 1

			for hit in sonyRe.findall(pageBody, re.I):
				print currentUrl + ",0,0,1,0,0,0,0" + \
					blankSection(9) + \
					blankOS(3)
				sonyCount += 1

			for hit in nokiaRe.findall(pageBody, re.I):
				print currentUrl + ",0,0,0,1,0,0,0" + \
					blankSection(9) + \
					blankOS(3)
				nokiaCount += 1

			for hit in htcRe.findall(pageBody, re.I):
				print currentUrl + ",0,0,0,0,1,0,0" + \
					blankSection(9) + \
					blankOS(3)
				htcCount += 1

			for hit in iosRe.findall(pageBody, re.I):
				print currentUrl + ",0,0,0,0,0,1,0" + \
					blankSection(9) + \
					blankOS(3)
				iosCount += 1

			for hit in googleRe.findall(pageBody, re.I):
				print currentUrl + ",0,0,0,0,0,0,1" + \
					blankSection(9) + \
					blankOS(3)
				googleCount += 1

			# If there were any topic matches found
			if((iphoneCount+samsungCount+sonyCount+nokiaCount+htcCount+iosCount+googleCount) > 0):

				# Look for sentiment matches
				for hit in camPosRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(8) + \
						blankOS(3)

				for hit in camNegRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(1) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(7) + \
						blankOS(3)

				for hit in camUncRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(2) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(6) + \
						blankOS(3)

				for hit in disPosRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(3) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(5) + \
						blankOS(3)

				for hit in disNegRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(4) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(4) + \
						blankOS(3)

				for hit in disUncRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(5) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(3) + \
						blankOS(3)

				for hit in perPosRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(6) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(2) + \
						blankOS(3)

				for hit in perNegRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(7) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankSection(1) + \
						blankOS(3)

				for hit in perUncRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(8) + "," + \
						("1," if iphoneCount>0 else "0,") + \
						("1," if samsungCount>0 else "0,") + \
						("1," if sonyCount>0 else "0,") + \
						("1," if nokiaCount>0 else "0,") + \
						("1" if htcCount>0 else "0") + \
						blankOS(3)

				for hit in OSPerPosRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(9) + "," + \
						("1," if iosCount>0 else "0,") + \
						("1" if googleCount>0 else "0") + \
						blankOS(2)

				for hit in OSPerNegRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(9) + \
						blankOS(1) + "," + \
						("1," if iosCount>0 else "0,") + \
						("1" if googleCount>0 else "0") + \
						blankOS(1)

				for hit in OSPerUncRe.findall(pageBody, re.I):
					print currentUrl + blankMain() + \
						blankSection(9) + \
						blankOS(2) + "," + \
						("1," if iosCount>0 else "0,") + \
						("1" if googleCount>0 else "0")

 			# Assign the new url header match value to a variable
#			currentUrl = pageHeaderMatch
			currentUrl = "\"" + pageHeaderMatch.group(1).strip().replace(",", "") + "\""
#			currentUrl = currentUrl.replace("http://", "")
#			currentUrl = currentUrl.replace("/", "-")
#			currentUrl = currentUrl.replace("\(|\)", "")
			pageBody = ""
		else:
			pageBody = pageBody + line

def blankMain():
	return ",0,0,0,0,0,0,0"

def blankSection(qty):
	returnString = ""
	for i in range(0, qty):
		returnString = returnString + ",0,0,0,0,0"
	return returnString

def blankOS(qty):
	returnString = ""
	for i in range(0, qty):
		returnString = returnString + ",0,0"
	return returnString

if __name__ == "__main__":
	main(sys.argv)