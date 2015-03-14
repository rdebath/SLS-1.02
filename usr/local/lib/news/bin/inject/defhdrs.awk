# defhdrs.awk
# pass 1 - note presence | absence of certain headers
# a header keyword: remember it and its value
BEGIN	{ status = 0 }
/^[^\t ]*:/ {
	hdrval[$1] = $0
	keyword=$1
	next
}
# a continuation: concatenate this line to the value
	{ hdrval[keyword] = hdrval[keyword] "\n" $0 }

END {
	# pass 2 - cogitate & omit & emit headers
	emptyhdrre = "^[^\t ]*:[\t ]*$"
	subjname = "Subject:"
	ctlname = "Control:"
	ngname = "Newsgroups:"
	msgidname = "Message-ID:"
	typoname =  "Message-Id:"
	pathname = "Path:"
	datename = "Date:"
	fromname = "From:"
	orgname = "Organization:"
	distrname = "Distribution:"
	sendername = "Sender:"
	expiresname = "Expires:"

	# nullify headers with empty contents
	for (i in hdrval)
		if (hdrval[i] ~ /^[^\t ]*:[\t ]*$/)
			hdrval[i] = ""

	# fill in missing headers
	if (hdrval[typoname] != "") {	# spelling hack
		hdrval[msgidname] = hdrval[typoname]
		hdrval[typoname] = ""
		# fix spelling: Message-Id: -> Message-ID:
		nf = split(hdrval[msgidname], fields);	# bust up
		fields[1] = msgidname;		# fix spelling
		hdrval[msgidname] = fields[1];	# reassemble...
		for (i = 2; i <= nf; i++)
			hdrval[msgidname] = hdrval[msgidname] " " fields[i]
	}
	if (hdrval[pathname] == "")
		hdrval[pathname] = pathname " " defpath
	if (hdrval[msgidname] == "")
		hdrval[msgidname] = msgidname " " defmsgid
	if (hdrval[datename] == "")
		hdrval[datename] = datename " " defdate
	if (hdrval[expiresname] == "" && defexpiry != "")
		hdrval[expiresname] = expiresname " " defexpiry
	if (hdrval[orgname] == "" && deforg != "")
		hdrval[orgname] = orgname " " deforg
	if (hdrval[fromname] == "")
		hdrval[fromname] = fromname " " deffrom
	else if (hdrval[sendername] == "")
		hdrval[sendername] = sendername " " deffrom

	# replace user's headers (if any) [this is not currently done]

	# snuff some headers
	distworld = distrname " world"
	if (hdrval[distrname] == distworld)
		hdrval[distrname] = ""

	# the vile cmsg hack, for the sake of the news readers *only*
	if (hdrval[ctlname] == "" && \
	    substr(hdrval[subjname], 1, 14) == "Subject: cmsg ")
		hdrval[ctlname] = ctlname " " substr(hdrval[subjname], 15)

	# warn if no Subject:
	if (hdrval[subjname] == "") {
		print "defhdrs.awk: no " subjname " header!" | "cat >&2"
		status = 1
	}

	# warn if no Newsgroups:
	if (hdrval[ngname] == "") {
		print "defhdrs.awk: no " ngname " header!" | "cat >&2"
		status = 1
	}
	if (hdrval[ngname] ~ /^Newsgroups:  *.*[\t ]/) {
		print "defhdrs.awk: whitespace in " ngname " header" | "cat >&2"
		status = 1
	}

	# field the all.all.ctl hack, for the sake of the backward only:
	# clone Subject: to make Control:
	if (hdrval[ctlname] == "" && hdrval[ngname] ~ /\.ctl(,|$)/)
		hdrval[ctlname] = ctlname " " substr(hdrval[subjname], 8)

	# rewrite "Path: ME!blah" to "Path: blah", repeatedly
	while (substr(hdrval[pathname], 1, length(me)+1) == (me "!"))
		hdrval[pathname] = substr(hdrval[pathname], length(me)+2)

	# reorder & emit headers

	# favour Control: & Newsgroups: for future benefit of rnews
	if (hdrval[ctlname] != "") {
		print hdrval[ctlname]
		hdrval[ctlname] = ""	# no Control: to print now
	}
	if (hdrval[ngname] != "") {
		print hdrval[ngname]
		hdrval[ngname] = ""	# no Newsgroups: to print now
	}

	# B inews kludgery: print Path: before From: to avoid confusing it
	if (hdrval[pathname] != "") {
		print hdrval[pathname]
		hdrval[pathname] = ""	# no Path: to print now
	}
	if (hdrval[fromname] != "") {
		print hdrval[fromname]
		hdrval[fromname] = ""	# no From: to print now
	}

	# have pity on readers: put Subject: next
	if (hdrval[subjname] != "") {
		print hdrval[subjname]
		hdrval[subjname] = ""	# no Subject: to print now
	}

	# print misc. non-empty headers in random order
	for (i in hdrval)
		if (hdrval[i] != "" && hdrval[i] !~ /^[^\t ]*:[\t ]*$/)
			print hdrval[i]

	exit status
}
