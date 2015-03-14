divert(1)
diversion 1, line 1
diversion 1, line 2
define(`foo',
``foo' expansion')
diversion 1, line 5
diversion 1, line 6
foo
divert

Here is the undiverted text (bug here): undivert(1) -- Thats it.
