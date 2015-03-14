
"By Andy Valencia (uunet!sequent!vandys)"

"A source of random integers"

Integer subclass: #RandomInteger
	instanceVariableNames: ''
	classVariableNames: 'Source'
	poolDictionaries: ''
	category: nil.
!

RandomInteger comment:
'My instances are random integers'
!

!RandomInteger class methodsFor: 'instance creation'!

new
	^ self error: 'Must use between:and:'
!

between: low and: high
	| i range |

	(Source = nil) ifTrue: [ Source _ Random new ].
	range _ high - low.
	i _ (((Source next) * (range + 1)) + low) rounded.
	^ i
!!

