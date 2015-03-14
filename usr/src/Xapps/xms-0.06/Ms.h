/* Ms.h - public header for MandelSpawn popup widget */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

#ifndef _Ms_h
#define _Ms_h

#define XtRDouble 	"Double"

#define XtNiteration_limit 	"iteration_limit"
#define XtNCenterX 	"center_x"
#define XtNCenterY 	"center_y"
#define XtNRange 	"range"
#define XtNInside 	"inside"
#define XtNCenterBox 	"center_box"
#define XtNCursor 	"cursor"
#define XtNMama		"mama"
#define XtNJulia	"julia"
#define XtNCX 		"c_x"
#define XtNCY 		"c_y"
#define XtNChunkWidth	"chunk_width"
#define XtNChunkHeight	"chunk_height"
#define XtNSony		"sony_bug_workaround"
#define XtNCrosshairSize "crosshair_size"
#define XtNInterior	"interior"

typedef struct _MsRec *MsWidget;
typedef struct _MsClassRec *MsWidgetClass;

extern WidgetClass msWidgetClass;

/* public functions */
void Draw();
struct static_job_info GetJobInfo();

#endif _Ms_h
