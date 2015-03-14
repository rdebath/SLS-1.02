# canonicalise the sys file:
# delete comments & leading whitespace, collapse continued lines
# rewritten to avoid assignment to $0, which is broken in older awks
/^/	{ thisln = $0 }
/^#/	{ partline = ""; next }		# delete comments
/^[\t ]/	{
	n = 0
	for (s = substr(thisln, n); s ~ /^[\t ]/; s = substr(thisln, ++n))
		;			# skip leading whitespace
	thisln = s
}
/\\$/	{ partline = partline substr(thisln, 1, length(thisln)-1); next }
{					# non-continued line
	partline = partline thisln	# terminate the whole entry
	if (partline != "")
		print partline
	partline = ""
}
END	{
	if (partline != "")
		print partline		# flush any partial line
}
