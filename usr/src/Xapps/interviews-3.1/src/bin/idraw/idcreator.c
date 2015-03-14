/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Idraw object constructor class implementation.
 */

#include "idarrow.h"
#include "idclasses.h"
#include "idcmds.h"
#include "idcomp.h"
#include "idcreator.h"
#include "idvars.h"

#include <Unidraw/catalog.h>

/*****************************************************************************/

IdrawCreator::IdrawCreator () { }

void* IdrawCreator::Create (
    ClassId id, istream& in, ObjectMap* objmap, int objid
) {
    switch (id) {
        case ABOUT_CMD:            CREATE(AboutCmd, in, objmap, objid);
        case ARROWLINE_COMP:       CREATE(ArrowLineComp, in, objmap, objid);
        case ARROWMULTILINE_COMP:  CREATE(ArrowMultiLineComp, in,objmap,objid);
        case ARROWSPLINE_COMP:     CREATE(ArrowSplineComp, in, objmap, objid);
        case ARROW_CMD:            CREATE(ArrowCmd, in, objmap, objid);
        case ARROW_VAR:            CREATE(ArrowVar, in, objmap, objid);
        case IDRAW_COMP:           CREATE(IdrawComp, in, objmap, objid);
        case IGRIDSPACING_CMD:     CREATE(IGridSpacingCmd, in, objmap, objid);
        case NEWVIEW_CMD:          CREATE(NewViewCmd, in, objmap, objid);
        case OPEN_CMD:             CREATE(OpenCmd, in, objmap, objid);
        case PRECISEMOVE_CMD:      CREATE(PreciseMoveCmd, in, objmap, objid);
        case PRECISEROTATE_CMD:    CREATE(PreciseRotateCmd, in, objmap, objid);
        case PRECISESCALE_CMD:     CREATE(PreciseScaleCmd, in, objmap, objid);

        default:                   return Creator::Create(id, in,objmap,objid);
    }
}

void* IdrawCreator::Create (ClassId id) {
    if (id == ARROWLINE_VIEW)      return new ArrowLineView;
    if (id == ARROWMULTILINE_VIEW) return new ArrowMultiLineView;
    if (id == ARROWSPLINE_VIEW)    return new ArrowSplineView;
    if (id == IDRAW_VIEW)          return new IdrawView;
    if (id == PS_ARROWLINE)        return new PSArrowLine;
    if (id == PS_ARROWMULTILINE)   return new PSArrowMultiLine;
    if (id == PS_ARROWSPLINE)      return new PSArrowSpline;
    if (id == PS_IDRAW)            return new PSIdraw;

    return Creator::Create(id);
}
