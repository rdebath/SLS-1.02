//
// For testing how various math libraries will respond
//

diary();

Inf = inf();
NaN = nan();

//
// Trig Functions
//

cos( Inf )
sin( Inf )
tan( Inf )
acos( Inf )
asin( Inf )
atan( Inf )

cos( -Inf )
sin( -Inf )
tan( -Inf )
acos( -Inf )
asin( -Inf )
atan( -Inf )

cos( NaN )
sin( NaN )
tan( NaN )
acos( NaN )
asin( NaN )
atan( NaN )

//
// Hyperbolic trig functions
//

sinh( Inf )
cosh( Inf )
tanh( Inf )

sinh( -Inf )
cosh( -Inf )
tanh( -Inf )

sinh( NaN )
cosh( NaN )
tanh( NaN )

//
// Misc.
//

exp( Inf )
log( Inf )
log10( Inf )
sqrt( Inf )

exp( -Inf )
log( -Inf )
log10( -Inf )
sqrt( -Inf )

exp( NaN )
log( NaN )
log10( NaN )
sqrt( NaN )
