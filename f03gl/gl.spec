# gl.spec file
# DON'T REMOVE PREVIOUS LINE!!! libspec depends on it!
#
# License Applicability. Except to the extent portions of this file are
# made subject to an alternative license as permitted in the SGI Free
# Software License B, Version 1.1 (the "License"), the contents of this
# file are subject only to the provisions of the License. You may not use
# this file except in compliance with the License. You may obtain a copy
# of the License at Silicon Graphics, Inc., attn: Legal Services, 1600
# Amphitheatre Parkway, Mountain View, CA 94043-1351, or at:
#
# http://oss.sgi.com/projects/FreeB
#
# Note that, as provided in the License, the Software is distributed on an
# "AS IS" basis, with ALL EXPRESS AND IMPLIED WARRANTIES AND CONDITIONS
# DISCLAIMED, INCLUDING, WITHOUT LIMITATION, ANY IMPLIED WARRANTIES AND
# CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A
# PARTICULAR PURPOSE, AND NON-INFRINGEMENT.
#
# Original Code. The Original Code is: OpenGL Sample Implementation,
# Version 1.2.1, released January 26, 2000, developed by Silicon Graphics,
# Inc. The Original Code is Copyright (c) 1991-2000 Silicon Graphics, Inc.
# Copyright in any portions created by third parties is as indicated
# elsewhere herein. All Rights Reserved.
#
# Additional Notice Provisions: The application programming interfaces
# established by SGI in conjunction with the Original Code are The
# OpenGL(R) Graphics System: A Specification (Version 1.2.1), released
# April 1, 1999; The OpenGL(R) Graphics System Utility Library (Version
# 1.3), released November 4, 1998; and OpenGL(R) Graphics with the X
# Window System(R) (Version 1.3), released October 19, 1998. This software
# was created using the OpenGL(R) version 1.2.1 Sample Implementation
# published by SGI, but has not been independently verified as being
# compliant with the OpenGL(R) version 1.2.1 Specification.
#

# $Date$ $Revision$
# $Header: //depot/main/gfx/include/gl/spec/gl.spec#21 $

required-props:
param:		retval retained
dlflags:	notlistable handcode
glxflags:	client-intercept client-handcode server-handcode EXT SGI ARB ignore
vectorequiv:	*
category:	display-list drawing drawing-control feedback framebuf misc modeling pixel-op pixel-rw state-req xform 1_1 1_2 ARB_multitexture EXT_color_subtable EXT_color_table EXT_compiled_vertex_array EXT_vertex_array
version:	1.0 1.1 1.2
glxsingle:	*
glxropcode:	*
glxvendorpriv:	*
glfflags:	capture-execute capture-handcode decode-handcode gl-enum matrix pixel-pack pixel-unpack store-return ignore
glfoffset:	*
glfpage:	*
glsflags:	capture-handcode client-get client-state get gl-enum ignore matrix pixel-null pixel-pack pixel-unpack
glsoffset:	*
glspage:	*
offset:		*
wglflags:	ignore
extension:	not_implemented soft WINSOFT

###############################################################################
#
# GLX opcodes
#	glxsingle:		101-159     (1.0-1.2 core)
#				160	    (ARB_texture_compression)
#	glxropcode:		1-196	    (1.2 core; ropcode 140 unused?!)
#				197-213     (ARB_multitexture)
#				214-219     (ARB_texture_compression)
#				2048-2081   (SGI extensions)
#				4096-4123   (1.2 core and multivendor EXT)
#				4124-4142   (EXT extensions)
#
# When reserving new glxropcodes, update in oddball:
#     gfx/lib/opengl/doc/glspec/extensions.reserved to indicate this
#
###############################################################################

###############################################################################
#
# things to remember when adding an extension command
#
# - Add new commands at end of file
# - use tabs, not spaces
# - set glfflags to "ignore"
# - set glsflags to "ignore" until GLS is updated to support the new command
# - set glxflags to "ignore" until GLX is updated to support the new command
# - add new data types to typemaps/spec2wire.map
# - add extension name in alphabetical order to category list
# - add a batch of extensions in alphabetical order by extension name
# - add commands within an extension in alphabetical order
# - use existing command entries as a model (where possible)
#
###############################################################################

###############################################################################
#
# display-list commands
#
###############################################################################

NewList(list, mode)
	return		void
	param		list		List in value
	param		mode		ListMode in value
	dlflags		notlistable
	category	display-list
	version		1.0
	glxsingle	101
	glfflags	capture-handcode decode-handcode
	glfoffset	0
	glfpage		3
	glsoffset	0
	glspage		3
	offset		0

EndList()
	return		void
	dlflags		notlistable
	category	display-list
	version		1.0
	glxsingle	102
	glfoffset	1
	glfpage		3
	glsoffset	1
	glspage		3
	offset		1

CallList(list)
	return		void
	param		list		List in value
	category	display-list
	version		1.0
	glxropcode	1
	glfoffset	2
	glfpage		3
	glsoffset	2
	glspage		3
	offset		2

CallLists(n, type, lists)
	return		void
	param		n		SizeI in value
	param		type		ListNameType in value
	param		lists		Void in array [COMPSIZE(n/type)]
	category	display-list
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	2
	glfflags	capture-handcode decode-handcode
	glfoffset	3
	glfpage		3
	glsoffset	3
	glspage		3
	offset		3

DeleteLists(list, range)
	return		void
	param		list		List in value
	param		range		SizeI in value
	dlflags		notlistable
	category	display-list
	version		1.0
	glxsingle	103
	glfoffset	4
	glfpage		3
	glsoffset	4
	glspage		3
	offset		4

GenLists(range)
	return		List
	param		range		SizeI in value
	dlflags		notlistable
	category	display-list
	version		1.0
	glxsingle	104
	glfflags	capture-execute store-return
	glfoffset	5
	glfpage		3
	glsoffset	5
	glspage		3
	offset		5

ListBase(base)
	return		void
	param		base		List in value
	category	display-list
	version		1.0
	glxropcode	3
	glfoffset	6
	glfpage		3
	glsoffset	6
	glspage		3
	offset		6

###############################################################################
#
# drawing commands
#
###############################################################################

Begin(mode)
	return		void
	param		mode		BeginMode in value
	category	drawing
	version		1.0
	glxropcode	4
	glfoffset	7
	glfpage		3
	glsoffset	7
	glspage		3
	offset		7

Bitmap(width, height, xorig, yorig, xmove, ymove, bitmap)
	return		void
	param		width		SizeI in value
	param		height		SizeI in value
	param		xorig		CoordF in value
	param		yorig		CoordF in value
	param		xmove		CoordF in value
	param		ymove		CoordF in value
	param		bitmap		UInt8 in array [COMPSIZE(width/height)]
	category	drawing
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	5
	glfflags	pixel-unpack
	glfoffset	8
	glfpage		3
	glsflags	pixel-unpack
	glsoffset	8
	glspage		3
	offset		8

Color3b(red, green, blue)
	return		void
	param		red		ColorB in value
	param		green		ColorB in value
	param		blue		ColorB in value
	category	drawing
	vectorequiv	Color3bv
	version		1.0
	glfoffset	9
	glfpage		3
	offset		9

Color3bv(v)
	return		void
	param		v		ColorB in array [3]
	category	drawing
	version		1.0
	glxropcode	6
	glfoffset	10
	glfpage		3
	glsoffset	9
	glspage		3
	offset		10

Color3d(red, green, blue)
	return		void
	param		red		ColorD in value
	param		green		ColorD in value
	param		blue		ColorD in value
	category	drawing
	vectorequiv	Color3dv
	version		1.0
	glfoffset	11
	glfpage		3
	offset		11

Color3dv(v)
	return		void
	param		v		ColorD in array [3]
	category	drawing
	version		1.0
	glxropcode	7
	glfoffset	12
	glfpage		3
	glsoffset	10
	glspage		3
	offset		12

Color3f(red, green, blue)
	return		void
	param		red		ColorF in value
	param		green		ColorF in value
	param		blue		ColorF in value
	category	drawing
	vectorequiv	Color3fv
	version		1.0
	glfoffset	13
	glfpage		3
	offset		13

Color3fv(v)
	return		void
	param		v		ColorF in array [3]
	category	drawing
	version		1.0
	glxropcode	8
	glfoffset	14
	glfpage		3
	glsoffset	11
	glspage		3
	offset		14

Color3i(red, green, blue)
	return		void
	param		red		ColorI in value
	param		green		ColorI in value
	param		blue		ColorI in value
	category	drawing
	vectorequiv	Color3iv
	version		1.0
	glfoffset	15
	glfpage		3
	offset		15

Color3iv(v)
	return		void
	param		v		ColorI in array [3]
	category	drawing
	version		1.0
	glxropcode	9
	glfoffset	16
	glfpage		3
	glsoffset	12
	glspage		3
	offset		16

Color3s(red, green, blue)
	return		void
	param		red		ColorS in value
	param		green		ColorS in value
	param		blue		ColorS in value
	category	drawing
	vectorequiv	Color3sv
	version		1.0
	glfoffset	17
	glfpage		3
	offset		17

Color3sv(v)
	return		void
	param		v		ColorS in array [3]
	category	drawing
	version		1.0
	glxropcode	10
	glfoffset	18
	glfpage		3
	glsoffset	13
	glspage		3
	offset		18

Color3ub(red, green, blue)
	return		void
	param		red		ColorUB in value
	param		green		ColorUB in value
	param		blue		ColorUB in value
	category	drawing
	vectorequiv	Color3ubv
	version		1.0
	glfoffset	19
	glfpage		3
	offset		19

Color3ubv(v)
	return		void
	param		v		ColorUB in array [3]
	category	drawing
	version		1.0
	glxropcode	11
	glfoffset	20
	glfpage		3
	glsoffset	14
	glspage		3
	offset		20

Color3ui(red, green, blue)
	return		void
	param		red		ColorUI in value
	param		green		ColorUI in value
	param		blue		ColorUI in value
	category	drawing
	vectorequiv	Color3uiv
	version		1.0
	glfoffset	21
	glfpage		3
	offset		21

Color3uiv(v)
	return		void
	param		v		ColorUI in array [3]
	category	drawing
	version		1.0
	glxropcode	12
	glfoffset	22
	glfpage		3
	glsoffset	15
	glspage		3
	offset		22

Color3us(red, green, blue)
	return		void
	param		red		ColorUS in value
	param		green		ColorUS in value
	param		blue		ColorUS in value
	category	drawing
	vectorequiv	Color3usv
	version		1.0
	glfoffset	23
	glfpage		3
	offset		23

Color3usv(v)
	return		void
	param		v		ColorUS in array [3]
	category	drawing
	version		1.0
	glxropcode	13
	glfoffset	24
	glfpage		3
	glsoffset	0
	glspage		4
	offset		24

Color4b(red, green, blue, alpha)
	return		void
	param		red		ColorB in value
	param		green		ColorB in value
	param		blue		ColorB in value
	param		alpha		ColorB in value
	category	drawing
	vectorequiv	Color4bv
	version		1.0
	glfoffset	25
	glfpage		3
	offset		25

Color4bv(v)
	return		void
	param		v		ColorB in array [4]
	category	drawing
	version		1.0
	glxropcode	14
	glfoffset	26
	glfpage		3
	glsoffset	1
	glspage		4
	offset		26

Color4d(red, green, blue, alpha)
	return		void
	param		red		ColorD in value
	param		green		ColorD in value
	param		blue		ColorD in value
	param		alpha		ColorD in value
	category	drawing
	vectorequiv	Color4dv
	version		1.0
	glfoffset	27
	glfpage		3
	offset		27

Color4dv(v)
	return		void
	param		v		ColorD in array [4]
	category	drawing
	version		1.0
	glxropcode	15
	glfoffset	28
	glfpage		3
	glsoffset	2
	glspage		4
	offset		28

Color4f(red, green, blue, alpha)
	return		void
	param		red		ColorF in value
	param		green		ColorF in value
	param		blue		ColorF in value
	param		alpha		ColorF in value
	category	drawing
	vectorequiv	Color4fv
	version		1.0
	glfoffset	29
	glfpage		3
	offset		29

Color4fv(v)
	return		void
	param		v		ColorF in array [4]
	category	drawing
	version		1.0
	glxropcode	16
	glfoffset	30
	glfpage		3
	glsoffset	3
	glspage		4
	offset		30

Color4i(red, green, blue, alpha)
	return		void
	param		red		ColorI in value
	param		green		ColorI in value
	param		blue		ColorI in value
	param		alpha		ColorI in value
	category	drawing
	vectorequiv	Color4iv
	version		1.0
	glfoffset	31
	glfpage		3
	offset		31

Color4iv(v)
	return		void
	param		v		ColorI in array [4]
	category	drawing
	version		1.0
	glxropcode	17
	glfoffset	32
	glfpage		3
	glsoffset	4
	glspage		4
	offset		32

Color4s(red, green, blue, alpha)
	return		void
	param		red		ColorS in value
	param		green		ColorS in value
	param		blue		ColorS in value
	param		alpha		ColorS in value
	category	drawing
	vectorequiv	Color4sv
	version		1.0
	glfoffset	33
	glfpage		3
	offset		33

Color4sv(v)
	return		void
	param		v		ColorS in array [4]
	category	drawing
	version		1.0
	glxropcode	18
	glfoffset	34
	glfpage		3
	glsoffset	5
	glspage		4
	offset		34

Color4ub(red, green, blue, alpha)
	return		void
	param		red		ColorUB in value
	param		green		ColorUB in value
	param		blue		ColorUB in value
	param		alpha		ColorUB in value
	category	drawing
	vectorequiv	Color4ubv
	version		1.0
	glfoffset	35
	glfpage		3
	offset		35

Color4ubv(v)
	return		void
	param		v		ColorUB in array [4]
	category	drawing
	version		1.0
	glxropcode	19
	glfoffset	36
	glfpage		3
	glsoffset	6
	glspage		4
	offset		36

Color4ui(red, green, blue, alpha)
	return		void
	param		red		ColorUI in value
	param		green		ColorUI in value
	param		blue		ColorUI in value
	param		alpha		ColorUI in value
	category	drawing
	vectorequiv	Color4uiv
	version		1.0
	glfoffset	37
	glfpage		3
	offset		37

Color4uiv(v)
	return		void
	param		v		ColorUI in array [4]
	category	drawing
	version		1.0
	glxropcode	20
	glfoffset	38
	glfpage		3
	glsoffset	7
	glspage		4
	offset		38

Color4us(red, green, blue, alpha)
	return		void
	param		red		ColorUS in value
	param		green		ColorUS in value
	param		blue		ColorUS in value
	param		alpha		ColorUS in value
	category	drawing
	vectorequiv	Color4usv
	version		1.0
	glfoffset	39
	glfpage		3
	offset		39

Color4usv(v)
	return		void
	param		v		ColorUS in array [4]
	category	drawing
	version		1.0
	glxropcode	21
	glfoffset	40
	glfpage		3
	glsoffset	8
	glspage		4
	offset		40

EdgeFlag(flag)
	return		void
	param		flag		Boolean in value
	category	drawing
	vectorequiv	EdgeFlagv
	version		1.0
	glfoffset	41
	glfpage		3
	offset		41

EdgeFlagv(flag)
	return		void
	param		flag		Boolean in array [1]
	category	drawing
	version		1.0
	glxropcode	22
	glfoffset	42
	glfpage		3
	glsoffset	9
	glspage		4
	offset		42

End()
	return		void
	category	drawing
	version		1.0
	glxropcode	23
	glfoffset	43
	glfpage		3
	glsoffset	10
	glspage		4
	offset		43

Indexd(c)
	return		void
	param		c		ColorIndexValueD in value
	category	drawing
	vectorequiv	Indexdv
	version		1.0
	glfoffset	44
	glfpage		3
	offset		44

Indexdv(c)
	return		void
	param		c		ColorIndexValueD in array [1]
	category	drawing
	version		1.0
	glxropcode	24
	glfoffset	45
	glfpage		3
	glsoffset	11
	glspage		4
	offset		45

Indexf(c)
	return		void
	param		c		ColorIndexValueF in value
	category	drawing
	vectorequiv	Indexfv
	version		1.0
	glfoffset	46
	glfpage		3
	offset		46

Indexfv(c)
	return		void
	param		c		ColorIndexValueF in array [1]
	category	drawing
	version		1.0
	glxropcode	25
	glfoffset	47
	glfpage		3
	glsoffset	12
	glspage		4
	offset		47

Indexi(c)
	return		void
	param		c		ColorIndexValueI in value
	category	drawing
	vectorequiv	Indexiv
	version		1.0
	glfoffset	48
	glfpage		3
	offset		48

Indexiv(c)
	return		void
	param		c		ColorIndexValueI in array [1]
	category	drawing
	version		1.0
	glxropcode	26
	glfoffset	49
	glfpage		3
	glsoffset	13
	glspage		4
	offset		49

Indexs(c)
	return		void
	param		c		ColorIndexValueS in value
	category	drawing
	vectorequiv	Indexsv
	version		1.0
	glfoffset	50
	glfpage		3
	offset		50

Indexsv(c)
	return		void
	param		c		ColorIndexValueS in array [1]
	category	drawing
	version		1.0
	glxropcode	27
	glfoffset	51
	glfpage		3
	glsoffset	14
	glspage		4
	offset		51

Normal3b(nx, ny, nz)
	return		void
	param		nx		Int8 in value
	param		ny		Int8 in value
	param		nz		Int8 in value
	category	drawing
	vectorequiv	Normal3bv
	version		1.0
	glfoffset	52
	glfpage		3
	offset		52

Normal3bv(v)
	return		void
	param		v		Int8 in array [3]
	category	drawing
	version		1.0
	glxropcode	28
	glfoffset	53
	glfpage		3
	glsoffset	15
	glspage		4
	offset		53

Normal3d(nx, ny, nz)
	return		void
	param		nx		CoordD in value
	param		ny		CoordD in value
	param		nz		CoordD in value
	category	drawing
	vectorequiv	Normal3dv
	version		1.0
	glfoffset	54
	glfpage		3
	offset		54

Normal3dv(v)
	return		void
	param		v		CoordD in array [3]
	category	drawing
	version		1.0
	glxropcode	29
	glfoffset	55
	glfpage		3
	glsoffset	0
	glspage		5
	offset		55

Normal3f(nx, ny, nz)
	return		void
	param		nx		CoordF in value
	param		ny		CoordF in value
	param		nz		CoordF in value
	category	drawing
	vectorequiv	Normal3fv
	version		1.0
	glfoffset	56
	glfpage		3
	offset		56

Normal3fv(v)
	return		void
	param		v		CoordF in array [3]
	category	drawing
	version		1.0
	glxropcode	30
	glfoffset	57
	glfpage		3
	glsoffset	1
	glspage		5
	offset		57

Normal3i(nx, ny, nz)
	return		void
	param		nx		Int32 in value
	param		ny		Int32 in value
	param		nz		Int32 in value
	category	drawing
	vectorequiv	Normal3iv
	version		1.0
	glfoffset	58
	glfpage		3
	offset		58

Normal3iv(v)
	return		void
	param		v		Int32 in array [3]
	category	drawing
	version		1.0
	glxropcode	31
	glfoffset	59
	glfpage		3
	glsoffset	2
	glspage		5
	offset		59

Normal3s(nx, ny, nz)
	return		void
	param		nx		Int16 in value
	param		ny		Int16 in value
	param		nz		Int16 in value
	category	drawing
	vectorequiv	Normal3sv
	version		1.0
	glfoffset	60
	glfpage		3
	offset		60

Normal3sv(v)
	return		void
	param		v		Int16 in array [3]
	category	drawing
	version		1.0
	glxropcode	32
	glfoffset	61
	glfpage		3
	glsoffset	3
	glspage		5
	offset		61

RasterPos2d(x, y)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	category	drawing
	vectorequiv	RasterPos2dv
	version		1.0
	glfoffset	62
	glfpage		3
	offset		62

RasterPos2dv(v)
	return		void
	param		v		CoordD in array [2]
	category	drawing
	version		1.0
	glxropcode	33
	glfoffset	63
	glfpage		3
	glsoffset	4
	glspage		5
	offset		63

RasterPos2f(x, y)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	category	drawing
	vectorequiv	RasterPos2fv
	version		1.0
	glfoffset	0
	glfpage		4
	offset		64

RasterPos2fv(v)
	return		void
	param		v		CoordF in array [2]
	category	drawing
	version		1.0
	glxropcode	34
	glfoffset	1
	glfpage		4
	glsoffset	5
	glspage		5
	offset		65

RasterPos2i(x, y)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	category	drawing
	vectorequiv	RasterPos2iv
	version		1.0
	glfoffset	2
	glfpage		4
	offset		66

RasterPos2iv(v)
	return		void
	param		v		CoordI in array [2]
	category	drawing
	version		1.0
	glxropcode	35
	glfoffset	3
	glfpage		4
	glsoffset	6
	glspage		5
	offset		67

RasterPos2s(x, y)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	category	drawing
	vectorequiv	RasterPos2sv
	version		1.0
	glfoffset	4
	glfpage		4
	offset		68

RasterPos2sv(v)
	return		void
	param		v		CoordS in array [2]
	category	drawing
	version		1.0
	glxropcode	36
	glfoffset	5
	glfpage		4
	glsoffset	7
	glspage		5
	offset		69

RasterPos3d(x, y, z)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	param		z		CoordD in value
	vectorequiv	RasterPos3dv
	category	drawing
	version		1.0
	glfoffset	6
	glfpage		4
	offset		70

RasterPos3dv(v)
	return		void
	param		v		CoordD in array [3]
	category	drawing
	version		1.0
	glxropcode	37
	glfoffset	7
	glfpage		4
	glsoffset	8
	glspage		5
	offset		71

RasterPos3f(x, y, z)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	param		z		CoordF in value
	category	drawing
	vectorequiv	RasterPos3fv
	version		1.0
	glfoffset	8
	glfpage		4
	offset		72

RasterPos3fv(v)
	return		void
	param		v		CoordF in array [3]
	category	drawing
	version		1.0
	glxropcode	38
	glfoffset	9
	glfpage		4
	glsoffset	9
	glspage		5
	offset		73

RasterPos3i(x, y, z)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	param		z		CoordI in value
	category	drawing
	vectorequiv	RasterPos3iv
	version		1.0
	glfoffset	10
	glfpage		4
	offset		74

RasterPos3iv(v)
	return		void
	param		v		CoordI in array [3]
	category	drawing
	version		1.0
	glxropcode	39
	glfoffset	11
	glfpage		4
	glsoffset	10
	glspage		5
	offset		75

RasterPos3s(x, y, z)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	param		z		CoordS in value
	category	drawing
	vectorequiv	RasterPos3sv
	version		1.0
	glfoffset	12
	glfpage		4
	offset		76

RasterPos3sv(v)
	return		void
	param		v		CoordS in array [3]
	category	drawing
	version		1.0
	glxropcode	40
	glfoffset	13
	glfpage		4
	glsoffset	11
	glspage		5
	offset		77

RasterPos4d(x, y, z, w)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	param		z		CoordD in value
	param		w		CoordD in value
	vectorequiv	RasterPos4dv
	category	drawing
	version		1.0
	glfoffset	14
	glfpage		4
	offset		78

RasterPos4dv(v)
	return		void
	param		v		CoordD in array [4]
	category	drawing
	version		1.0
	glxropcode	41
	glfoffset	15
	glfpage		4
	glsoffset	12
	glspage		5
	offset		79

RasterPos4f(x, y, z, w)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	param		z		CoordF in value
	param		w		CoordF in value
	category	drawing
	vectorequiv	RasterPos4fv
	version		1.0
	glfoffset	16
	glfpage		4
	offset		80

RasterPos4fv(v)
	return		void
	param		v		CoordF in array [4]
	category	drawing
	version		1.0
	glxropcode	42
	glfoffset	17
	glfpage		4
	glsoffset	13
	glspage		5
	offset		81

RasterPos4i(x, y, z, w)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	param		z		CoordI in value
	param		w		CoordI in value
	category	drawing
	vectorequiv	RasterPos4iv
	version		1.0
	glfoffset	18
	glfpage		4
	offset		82

RasterPos4iv(v)
	return		void
	param		v		CoordI in array [4]
	category	drawing
	version		1.0
	glxropcode	43
	glfoffset	19
	glfpage		4
	glsoffset	14
	glspage		5
	offset		83

RasterPos4s(x, y, z, w)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	param		z		CoordS in value
	param		w		CoordS in value
	category	drawing
	vectorequiv	RasterPos4sv
	version		1.0
	glfoffset	20
	glfpage		4
	offset		84

RasterPos4sv(v)
	return		void
	param		v		CoordS in array [4]
	category	drawing
	version		1.0
	glxropcode	44
	glfoffset	21
	glfpage		4
	glsoffset	15
	glspage		5
	offset		85

Rectd(x1, y1, x2, y2)
	return		void
	param		x1		CoordD in value
	param		y1		CoordD in value
	param		x2		CoordD in value
	param		y2		CoordD in value
	category	drawing
	vectorequiv	Rectdv
	version		1.0
	glfoffset	22
	glfpage		4
	offset		86

Rectdv(v1, v2)
	return		void
	param		v1		CoordD in array [2]
	param		v2		CoordD in array [2]
	category	drawing
	version		1.0
	glxropcode	45
	glfoffset	23
	glfpage		4
	glsoffset	0
	glspage		6
	offset		87

Rectf(x1, y1, x2, y2)
	return		void
	param		x1		CoordF in value
	param		y1		CoordF in value
	param		x2		CoordF in value
	param		y2		CoordF in value
	category	drawing
	vectorequiv	Rectfv
	version		1.0
	glfoffset	24
	glfpage		4
	offset		88

Rectfv(v1, v2)
	return		void
	param		v1		CoordF in array [2]
	param		v2		CoordF in array [2]
	category	drawing
	version		1.0
	glxropcode	46
	glfoffset	25
	glfpage		4
	glsoffset	1
	glspage		6
	offset		89

Recti(x1, y1, x2, y2)
	return		void
	param		x1		CoordI in value
	param		y1		CoordI in value
	param		x2		CoordI in value
	param		y2		CoordI in value
	category	drawing
	vectorequiv	Rectiv
	version		1.0
	glfoffset	26
	glfpage		4
	offset		90

Rectiv(v1, v2)
	return		void
	param		v1		CoordI in array [2]
	param		v2		CoordI in array [2]
	category	drawing
	version		1.0
	glxropcode	47
	glfoffset	27
	glfpage		4
	glsoffset	2
	glspage		6
	offset		91

Rects(x1, y1, x2, y2)
	return		void
	param		x1		CoordS in value
	param		y1		CoordS in value
	param		x2		CoordS in value
	param		y2		CoordS in value
	category	drawing
	vectorequiv	Rectsv
	version		1.0
	glfoffset	28
	glfpage		4
	offset		92

Rectsv(v1, v2)
	return		void
	param		v1		CoordS in array [2]
	param		v2		CoordS in array [2]
	category	drawing
	version		1.0
	glxropcode	48
	glfoffset	29
	glfpage		4
	glsoffset	3
	glspage		6
	offset		93

TexCoord1d(s)
	return		void
	param		s		CoordD in value
	category	drawing
	vectorequiv	TexCoord1dv
	version		1.0
	glfoffset	30
	glfpage		4
	offset		94

TexCoord1dv(v)
	return		void
	param		v		CoordD in array [1]
	category	drawing
	version		1.0
	glxropcode	49
	glfoffset	31
	glfpage		4
	glsoffset	4
	glspage		6
	offset		95

TexCoord1f(s)
	return		void
	param		s		CoordF in value
	category	drawing
	vectorequiv	TexCoord1fv
	version		1.0
	glfoffset	32
	glfpage		4
	offset		96

TexCoord1fv(v)
	return		void
	param		v		CoordF in array [1]
	category	drawing
	version		1.0
	glxropcode	50
	glfoffset	33
	glfpage		4
	glsoffset	5
	glspage		6
	offset		97

TexCoord1i(s)
	return		void
	param		s		CoordI in value
	category	drawing
	vectorequiv	TexCoord1iv
	version		1.0
	glfoffset	34
	glfpage		4
	offset		98

TexCoord1iv(v)
	return		void
	param		v		CoordI in array [1]
	category	drawing
	version		1.0
	glxropcode	51
	glfoffset	35
	glfpage		4
	glsoffset	6
	glspage		6
	offset		99

TexCoord1s(s)
	return		void
	param		s		CoordS in value
	category	drawing
	vectorequiv	TexCoord1sv
	version		1.0
	glfoffset	36
	glfpage		4
	offset		100

TexCoord1sv(v)
	return		void
	param		v		CoordS in array [1]
	category	drawing
	version		1.0
	glxropcode	52
	glfoffset	37
	glfpage		4
	glsoffset	7
	glspage		6
	offset		101

TexCoord2d(s, t)
	return		void
	param		s		CoordD in value
	param		t		CoordD in value
	category	drawing
	vectorequiv	TexCoord2dv
	version		1.0
	glfoffset	38
	glfpage		4
	offset		102

TexCoord2dv(v)
	return		void
	param		v		CoordD in array [2]
	category	drawing
	version		1.0
	glxropcode	53
	glfoffset	39
	glfpage		4
	glsoffset	8
	glspage		6
	offset		103

TexCoord2f(s, t)
	return		void
	param		s		CoordF in value
	param		t		CoordF in value
	category	drawing
	vectorequiv	TexCoord2fv
	version		1.0
	glfoffset	40
	glfpage		4
	offset		104

TexCoord2fv(v)
	return		void
	param		v		CoordF in array [2]
	category	drawing
	version		1.0
	glxropcode	54
	glfoffset	41
	glfpage		4
	glsoffset	9
	glspage		6
	offset		105

TexCoord2i(s, t)
	return		void
	param		s		CoordI in value
	param		t		CoordI in value
	category	drawing
	vectorequiv	TexCoord2iv
	version		1.0
	glfoffset	42
	glfpage		4
	offset		106

TexCoord2iv(v)
	return		void
	param		v		CoordI in array [2]
	category	drawing
	version		1.0
	glxropcode	55
	glfoffset	43
	glfpage		4
	glsoffset	10
	glspage		6
	offset		107

TexCoord2s(s, t)
	return		void
	param		s		CoordS in value
	param		t		CoordS in value
	category	drawing
	vectorequiv	TexCoord2sv
	version		1.0
	glfoffset	44
	glfpage		4
	offset		108

TexCoord2sv(v)
	return		void
	param		v		CoordS in array [2]
	category	drawing
	version		1.0
	glxropcode	56
	glfoffset	45
	glfpage		4
	glsoffset	11
	glspage		6
	offset		109

TexCoord3d(s, t, r)
	return		void
	param		s		CoordD in value
	param		t		CoordD in value
	param		r		CoordD in value
	category	drawing
	vectorequiv	TexCoord3dv
	version		1.0
	glfoffset	46
	glfpage		4
	offset		110

TexCoord3dv(v)
	return		void
	param		v		CoordD in array [3]
	category	drawing
	version		1.0
	glxropcode	57
	glfoffset	47
	glfpage		4
	glsoffset	12
	glspage		6
	offset		111

TexCoord3f(s, t, r)
	return		void
	param		s		CoordF in value
	param		t		CoordF in value
	param		r		CoordF in value
	category	drawing
	vectorequiv	TexCoord3fv
	version		1.0
	glfoffset	48
	glfpage		4
	offset		112

TexCoord3fv(v)
	return		void
	param		v		CoordF in array [3]
	category	drawing
	version		1.0
	glxropcode	58
	glfoffset	49
	glfpage		4
	glsoffset	13
	glspage		6
	offset		113

TexCoord3i(s, t, r)
	return		void
	param		s		CoordI in value
	param		t		CoordI in value
	param		r		CoordI in value
	category	drawing
	vectorequiv	TexCoord3iv
	version		1.0
	glfoffset	50
	glfpage		4
	offset		114

TexCoord3iv(v)
	return		void
	param		v		CoordI in array [3]
	category	drawing
	version		1.0
	glxropcode	59
	glfoffset	51
	glfpage		4
	glsoffset	14
	glspage		6
	offset		115

TexCoord3s(s, t, r)
	return		void
	param		s		CoordS in value
	param		t		CoordS in value
	param		r		CoordS in value
	category	drawing
	vectorequiv	TexCoord3sv
	version		1.0
	glfoffset	52
	glfpage		4
	offset		116

TexCoord3sv(v)
	return		void
	param		v		CoordS in array [3]
	category	drawing
	version		1.0
	glxropcode	60
	glfoffset	53
	glfpage		4
	glsoffset	15
	glspage		6
	offset		117

TexCoord4d(s, t, r, q)
	return		void
	param		s		CoordD in value
	param		t		CoordD in value
	param		r		CoordD in value
	param		q		CoordD in value
	category	drawing
	vectorequiv	TexCoord4dv
	version		1.0
	glfoffset	54
	glfpage		4
	offset		118

TexCoord4dv(v)
	return		void
	param		v		CoordD in array [4]
	category	drawing
	version		1.0
	glxropcode	61
	glfoffset	55
	glfpage		4
	glsoffset	0
	glspage		7
	offset		119

TexCoord4f(s, t, r, q)
	return		void
	param		s		CoordF in value
	param		t		CoordF in value
	param		r		CoordF in value
	param		q		CoordF in value
	category	drawing
	vectorequiv	TexCoord4fv
	version		1.0
	glfoffset	56
	glfpage		4
	offset		120

TexCoord4fv(v)
	return		void
	param		v		CoordF in array [4]
	category	drawing
	version		1.0
	glxropcode	62
	glfoffset	57
	glfpage		4
	glsoffset	1
	glspage		7
	offset		121

TexCoord4i(s, t, r, q)
	return		void
	param		s		CoordI in value
	param		t		CoordI in value
	param		r		CoordI in value
	param		q		CoordI in value
	category	drawing
	vectorequiv	TexCoord4iv
	version		1.0
	glfoffset	58
	glfpage		4
	offset		122

TexCoord4iv(v)
	return		void
	param		v		CoordI in array [4]
	category	drawing
	version		1.0
	glxropcode	63
	glfoffset	59
	glfpage		4
	glsoffset	2
	glspage		7
	offset		123

TexCoord4s(s, t, r, q)
	return		void
	param		s		CoordS in value
	param		t		CoordS in value
	param		r		CoordS in value
	param		q		CoordS in value
	category	drawing
	vectorequiv	TexCoord4sv
	version		1.0
	glfoffset	60
	glfpage		4
	offset		124

TexCoord4sv(v)
	return		void
	param		v		CoordS in array [4]
	category	drawing
	version		1.0
	glxropcode	64
	glfoffset	61
	glfpage		4
	glsoffset	3
	glspage		7
	offset		125

Vertex2d(x, y)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	category	drawing
	vectorequiv	Vertex2dv
	version		1.0
	glfoffset	62
	glfpage		4
	offset		126

Vertex2dv(v)
	return		void
	param		v		CoordD in array [2]
	category	drawing
	version		1.0
	glxropcode	65
	glfoffset	63
	glfpage		4
	glsoffset	4
	glspage		7
	offset		127

Vertex2f(x, y)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	category	drawing
	vectorequiv	Vertex2fv
	version		1.0
	glfoffset	0
	glfpage		5
	offset		128

Vertex2fv(v)
	return		void
	param		v		CoordF in array [2]
	category	drawing
	version		1.0
	glxropcode	66
	glfoffset	1
	glfpage		5
	glsoffset	5
	glspage		7
	offset		129

Vertex2i(x, y)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	category	drawing
	vectorequiv	Vertex2iv
	version		1.0
	glfoffset	2
	glfpage		5
	offset		130

Vertex2iv(v)
	return		void
	param		v		CoordI in array [2]
	category	drawing
	version		1.0
	glxropcode	67
	glfoffset	3
	glfpage		5
	glsoffset	6
	glspage		7
	offset		131

Vertex2s(x, y)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	category	drawing
	vectorequiv	Vertex2sv
	version		1.0
	glfoffset	4
	glfpage		5
	offset		132

Vertex2sv(v)
	return		void
	param		v		CoordS in array [2]
	category	drawing
	version		1.0
	glxropcode	68
	glfoffset	5
	glfpage		5
	glsoffset	7
	glspage		7
	offset		133

Vertex3d(x, y, z)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	param		z		CoordD in value
	category	drawing
	vectorequiv	Vertex3dv
	version		1.0
	glfoffset	6
	glfpage		5
	offset		134

Vertex3dv(v)
	return		void
	param		v		CoordD in array [3]
	category	drawing
	version		1.0
	glxropcode	69
	glfoffset	7
	glfpage		5
	glsoffset	8
	glspage		7
	offset		135

Vertex3f(x, y, z)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	param		z		CoordF in value
	category	drawing
	vectorequiv	Vertex3fv
	version		1.0
	glfoffset	8
	glfpage		5
	offset		136

Vertex3fv(v)
	return		void
	param		v		CoordF in array [3]
	category	drawing
	version		1.0
	glxropcode	70
	glfoffset	9
	glfpage		5
	glsoffset	9
	glspage		7
	offset		137

Vertex3i(x, y, z)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	param		z		CoordI in value
	category	drawing
	vectorequiv	Vertex3iv
	version		1.0
	glfoffset	10
	glfpage		5
	offset		138

Vertex3iv(v)
	return		void
	param		v		CoordI in array [3]
	category	drawing
	version		1.0
	glxropcode	71
	glfoffset	11
	glfpage		5
	glsoffset	10
	glspage		7
	offset		139

Vertex3s(x, y, z)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	param		z		CoordS in value
	category	drawing
	vectorequiv	Vertex3sv
	version		1.0
	glfoffset	12
	glfpage		5
	offset		140

Vertex3sv(v)
	return		void
	param		v		CoordS in array [3]
	category	drawing
	version		1.0
	glxropcode	72
	glfoffset	13
	glfpage		5
	glsoffset	11
	glspage		7
	offset		141

Vertex4d(x, y, z, w)
	return		void
	param		x		CoordD in value
	param		y		CoordD in value
	param		z		CoordD in value
	param		w		CoordD in value
	category	drawing
	vectorequiv	Vertex4dv
	version		1.0
	glfoffset	14
	glfpage		5
	offset		142

Vertex4dv(v)
	return		void
	param		v		CoordD in array [4]
	category	drawing
	version		1.0
	glxropcode	73
	glfoffset	15
	glfpage		5
	glsoffset	12
	glspage		7
	offset		143

Vertex4f(x, y, z, w)
	return		void
	param		x		CoordF in value
	param		y		CoordF in value
	param		z		CoordF in value
	param		w		CoordF in value
	category	drawing
	vectorequiv	Vertex4fv
	version		1.0
	glfoffset	16
	glfpage		5
	offset		144

Vertex4fv(v)
	return		void
	param		v		CoordF in array [4]
	category	drawing
	version		1.0
	glxropcode	74
	glfoffset	17
	glfpage		5
	glsoffset	13
	glspage		7
	offset		145

Vertex4i(x, y, z, w)
	return		void
	param		x		CoordI in value
	param		y		CoordI in value
	param		z		CoordI in value
	param		w		CoordI in value
	category	drawing
	vectorequiv	Vertex4iv
	version		1.0
	glfoffset	18
	glfpage		5
	offset		146

Vertex4iv(v)
	return		void
	param		v		CoordI in array [4]
	category	drawing
	version		1.0
	glxropcode	75
	glfoffset	19
	glfpage		5
	glsoffset	14
	glspage		7
	offset		147

Vertex4s(x, y, z, w)
	return		void
	param		x		CoordS in value
	param		y		CoordS in value
	param		z		CoordS in value
	param		w		CoordS in value
	category	drawing
	vectorequiv	Vertex4sv
	version		1.0
	glfoffset	20
	glfpage		5
	offset		148

Vertex4sv(v)
	return		void
	param		v		CoordS in array [4]
	category	drawing
	version		1.0
	glxropcode	76
	glfoffset	21
	glfpage		5
	glsoffset	15
	glspage		7
	offset		149

###############################################################################
#
# drawing-control commands
#
###############################################################################

ClipPlane(plane, equation)
	return		void
	param		plane		ClipPlaneName in value
	param		equation	Float64 in array [4]
	category	drawing-control
	version		1.0
	glxropcode	77
	glfoffset	22
	glfpage		5
	glsoffset	0
	glspage		8
	offset		150

ColorMaterial(face, mode)
	return		void
	param		face		MaterialFace in value
	param		mode		ColorMaterialParameter in value
	category	drawing-control
	version		1.0
	glxropcode	78
	glfoffset	23
	glfpage		5
	glsoffset	1
	glspage		8
	offset		151

CullFace(mode)
	return		void
	param		mode		CullFaceMode in value
	category	drawing-control
	version		1.0
	glxropcode	79
	glfoffset	24
	glfpage		5
	glsoffset	2
	glspage		8
	offset		152

Fogf(pname, param)
	return		void
	param		pname		FogParameter in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	80
	glfflags	gl-enum
	glfoffset	25
	glfpage		5
	glsflags	gl-enum
	glsoffset	3
	glspage		8
	offset		153

Fogfv(pname, params)
	return		void
	param		pname		FogParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	81
	glfflags	gl-enum
	glfoffset	26
	glfpage		5
	glsflags	gl-enum
	glsoffset	4
	glspage		8
	offset		154

Fogi(pname, param)
	return		void
	param		pname		FogParameter in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	82
	glfflags	gl-enum
	glfoffset	27
	glfpage		5
	glsflags	gl-enum
	glsoffset	5
	glspage		8
	offset		155

Fogiv(pname, params)
	return		void
	param		pname		FogParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	83
	glfflags	gl-enum
	glfoffset	28
	glfpage		5
	glsflags	gl-enum
	glsoffset	6
	glspage		8
	offset		156

FrontFace(mode)
	return		void
	param		mode		FrontFaceDirection in value
	category	drawing-control
	version		1.0
	glxropcode	84
	glfoffset	29
	glfpage		5
	glsoffset	7
	glspage		8
	offset		157

Hint(target, mode)
	return		void
	param		target		HintTarget in value
	param		mode		HintMode in value
	category	drawing-control
	version		1.0
	glxropcode	85
	glfoffset	30
	glfpage		5
	glsoffset	8
	glspage		8
	offset		158

Lightf(light, pname, param)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	86
	glfoffset	31
	glfpage		5
	glsoffset	9
	glspage		8
	offset		159

Lightfv(light, pname, params)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	87
	glfoffset	32
	glfpage		5
	glsoffset	10
	glspage		8
	offset		160

Lighti(light, pname, param)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	88
	glfoffset	33
	glfpage		5
	glsoffset	11
	glspage		8
	offset		161

Lightiv(light, pname, params)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	89
	glfoffset	34
	glfpage		5
	glsoffset	12
	glspage		8
	offset		162

LightModelf(pname, param)
	return		void
	param		pname		LightModelParameter in value
	param		param		Float32 in value
	category	drawing-control
	version		1.0
	glxropcode	90
	glfflags	gl-enum
	glfoffset	35
	glfpage		5
	glsflags	gl-enum
	glsoffset	13
	glspage		8
	offset		163

LightModelfv(pname, params)
	return		void
	param		pname		LightModelParameter in value
	param		params		Float32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	91
	glfflags	gl-enum
	glfoffset	36
	glfpage		5
	glsflags	gl-enum
	glsoffset	14
	glspage		8
	offset		164

LightModeli(pname, param)
	return		void
	param		pname		LightModelParameter in value
	param		param		Int32 in value
	category	drawing-control
	version		1.0
	glxropcode	92
	glfflags	gl-enum
	glfoffset	37
	glfpage		5
	glsflags	gl-enum
	glsoffset	15
	glspage		8
	offset		165

LightModeliv(pname, params)
	return		void
	param		pname		LightModelParameter in value
	param		params		Int32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	93
	glfflags	gl-enum
	glfoffset	38
	glfpage		5
	glsflags	gl-enum
	glsoffset	0
	glspage		9
	offset		166

LineStipple(factor, pattern)
	return		void
	param		factor		CheckedInt32 in value
	param		pattern		LineStipple in value
	category	drawing-control
	version		1.0
	glxropcode	94
	glfflags	capture-handcode
	glfoffset	39
	glfpage		5
	glsoffset	1
	glspage		9
	offset		167

LineWidth(width)
	return		void
	param		width		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	95
	glfoffset	40
	glfpage		5
	glsoffset	2
	glspage		9
	offset		168

Materialf(face, pname, param)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	96
	glfoffset	41
	glfpage		5
	glsoffset	3
	glspage		9
	offset		169

Materialfv(face, pname, params)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	97
	glfoffset	42
	glfpage		5
	glsoffset	4
	glspage		9
	offset		170

Materiali(face, pname, param)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	98
	glfoffset	43
	glfpage		5
	glsoffset	5
	glspage		9
	offset		171

Materialiv(face, pname, params)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	99
	glfoffset	44
	glfpage		5
	glsoffset	6
	glspage		9
	offset		172

PointSize(size)
	return		void
	param		size		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	100
	glfoffset	45
	glfpage		5
	glsoffset	7
	glspage		9
	offset		173

PolygonMode(face, mode)
	return		void
	param		face		MaterialFace in value
	param		mode		PolygonMode in value
	category	drawing-control
	version		1.0
	glxropcode	101
	glfoffset	46
	glfpage		5
	glsoffset	8
	glspage		9
	offset		174

PolygonStipple(mask)
	return		void
	param		mask		UInt8 in array [COMPSIZE()]
	category	drawing-control
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	102
	glfflags	pixel-unpack
	glfoffset	47
	glfpage		5
	glsflags	pixel-unpack
	glsoffset	9
	glspage		9
	offset		175

Scissor(x, y, width, height)
	return		void
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	category	drawing-control
	version		1.0
	glxropcode	103
	glfoffset	48
	glfpage		5
	glsoffset	10
	glspage		9
	offset		176

ShadeModel(mode)
	return		void
	param		mode		ShadingModel in value
	category	drawing-control
	version		1.0
	glxropcode	104
	glfoffset	49
	glfpage		5
	glsoffset	11
	glspage		9
	offset		177

TexParameterf(target, pname, param)
	return		void
	param		target		TextureTarget in value
	param		pname		TextureParameterName in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	105
	glfflags	gl-enum
	glfoffset	50
	glfpage		5
	glsflags	gl-enum
	glsoffset	12
	glspage		9
	offset		178

TexParameterfv(target, pname, params)
	return		void
	param		target		TextureTarget in value
	param		pname		TextureParameterName in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	106
	glfflags	gl-enum
	glfoffset	51
	glfpage		5
	glsflags	gl-enum
	glsoffset	13
	glspage		9
	offset		179

TexParameteri(target, pname, param)
	return		void
	param		target		TextureTarget in value
	param		pname		TextureParameterName in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	107
	glfflags	gl-enum
	glfoffset	52
	glfpage		5
	glsflags	gl-enum
	glsoffset	14
	glspage		9
	offset		180

TexParameteriv(target, pname, params)
	return		void
	param		target		TextureTarget in value
	param		pname		TextureParameterName in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	108
	glfflags	gl-enum
	glfoffset	53
	glfpage		5
	glsflags	gl-enum
	glsoffset	15
	glspage		9
	offset		181

TexImage1D(target, level, internalformat, width, border, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		internalformat	TextureComponentCount in value
	param		width		SizeI in value
	param		border		CheckedInt32 in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width)]
	category	drawing-control
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	109
	glfflags	capture-handcode decode-handcode pixel-unpack
	glfoffset	54
	glfpage		5
	glsflags	pixel-null pixel-unpack
	glsoffset	0
	glspage		10
	offset		182

TexImage2D(target, level, internalformat, width, height, border, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		internalformat	TextureComponentCount in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		border		CheckedInt32 in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width/height)]
	category	drawing-control
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	110
	glfflags	capture-handcode decode-handcode pixel-unpack
	glfoffset	55
	glfpage		5
	glsflags	pixel-null pixel-unpack
	glsoffset	1
	glspage		10
	offset		183

TexEnvf(target, pname, param)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	111
	glfflags	gl-enum
	glfoffset	56
	glfpage		5
	glsflags	gl-enum
	glsoffset	2
	glspage		10
	offset		184

TexEnvfv(target, pname, params)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	112
	glfflags	gl-enum
	glfoffset	57
	glfpage		5
	glsflags	gl-enum
	glsoffset	3
	glspage		10
	offset		185

TexEnvi(target, pname, param)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	113
	glfflags	gl-enum
	glfoffset	58
	glfpage		5
	glsflags	gl-enum
	glsoffset	4
	glspage		10
	offset		186

TexEnviv(target, pname, params)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	114
	glfflags	gl-enum
	glfoffset	59
	glfpage		5
	glsflags	gl-enum
	glsoffset	5
	glspage		10
	offset		187

TexGend(coord, pname, param)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		param		Float64 in value
	category	drawing-control
	version		1.0
	glxropcode	115
	glfflags	gl-enum
	glfoffset	60
	glfpage		5
	glsflags	gl-enum
	glsoffset	6
	glspage		10
	offset		188

TexGendv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		Float64 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	116
	glfflags	gl-enum
	glfoffset	61
	glfpage		5
	glsflags	gl-enum
	glsoffset	7
	glspage		10
	offset		189

TexGenf(coord, pname, param)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		param		CheckedFloat32 in value
	category	drawing-control
	version		1.0
	glxropcode	117
	glfflags	gl-enum
	glfoffset	62
	glfpage		5
	glsflags	gl-enum
	glsoffset	8
	glspage		10
	offset		190

TexGenfv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	118
	glfflags	gl-enum
	glfoffset	63
	glfpage		5
	glsflags	gl-enum
	glsoffset	9
	glspage		10
	offset		191

TexGeni(coord, pname, param)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		param		CheckedInt32 in value
	category	drawing-control
	version		1.0
	glxropcode	119
	glfflags	gl-enum
	glfoffset	0
	glfpage		6
	glsflags	gl-enum
	glsoffset	10
	glspage		10
	offset		192

TexGeniv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	drawing-control
	version		1.0
	glxropcode	120
	glfflags	gl-enum
	glfoffset	1
	glfpage		6
	glsflags	gl-enum
	glsoffset	11
	glspage		10
	offset		193

###############################################################################
#
# feedback commands
#
###############################################################################

FeedbackBuffer(size, type, buffer)
	return		void
	param		size		SizeI in value
	param		type		FeedbackType in value
	param		buffer		FeedbackElement out array [size] retained
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	category	feedback
	version		1.0
	glxsingle	105
	glfflags	capture-handcode decode-handcode
	glfoffset	2
	glfpage		6
	glsflags	client-state
	glsoffset	12
	glspage		10
	offset		194

SelectBuffer(size, buffer)
	return		void
	param		size		SizeI in value
	param		buffer		SelectName out array [size] retained
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	category	feedback
	version		1.0
	glxsingle	106
	glfflags	capture-handcode decode-handcode
	glfoffset	3
	glfpage		6
	glsflags	client-state
	glsoffset	13
	glspage		10
	offset		195

RenderMode(mode)
	return		Int32
	param		mode		RenderingMode in value
	category	feedback
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.0
	glxsingle	107
	glfflags	capture-execute store-return
	glfoffset	4
	glfpage		6
	glsoffset	14
	glspage		10
	offset		196

InitNames()
	return		void
	category	feedback
	version		1.0
	glxropcode	121
	glfoffset	5
	glfpage		6
	glsoffset	15
	glspage		10
	offset		197

LoadName(name)
	return		void
	param		name		SelectName in value
	category	feedback
	version		1.0
	glxropcode	122
	glfoffset	6
	glfpage		6
	glsoffset	0
	glspage		11
	offset		198

PassThrough(token)
	return		void
	param		token		FeedbackElement in value
	category	feedback
	version		1.0
	glxropcode	123
	glfoffset	7
	glfpage		6
	glsoffset	1
	glspage		11
	offset		199

PopName()
	return		void
	category	feedback
	version		1.0
	glxropcode	124
	glfoffset	8
	glfpage		6
	glsoffset	2
	glspage		11
	offset		200

PushName(name)
	return		void
	param		name		SelectName in value
	category	feedback
	version		1.0
	glxropcode	125
	glfoffset	9
	glfpage		6
	glsoffset	3
	glspage		11
	offset		201

###############################################################################
#
# framebuf commands
#
###############################################################################

DrawBuffer(mode)
	return		void
	param		mode		DrawBufferMode in value
	category	framebuf
	version		1.0
	glxropcode	126
	glfoffset	10
	glfpage		6
	glsoffset	4
	glspage		11
	offset		202

Clear(mask)
	return		void
	param		mask		ClearBufferMask in value
	category	framebuf
	version		1.0
	glxropcode	127
	glfoffset	11
	glfpage		6
	glsoffset	5
	glspage		11
	offset		203

ClearAccum(red, green, blue, alpha)
	return		void
	param		red		Float32 in value
	param		green		Float32 in value
	param		blue		Float32 in value
	param		alpha		Float32 in value
	category	framebuf
	version		1.0
	glxropcode	128
	glfoffset	12
	glfpage		6
	glsoffset	6
	glspage		11
	offset		204

ClearIndex(c)
	return		void
	param		c		MaskedColorIndexValueF in value
	category	framebuf
	version		1.0
	glxropcode	129
	glfoffset	13
	glfpage		6
	glsoffset	7
	glspage		11
	offset		205

ClearColor(red, green, blue, alpha)
	return		void
	param		red		ClampedColorF in value
	param		green		ClampedColorF in value
	param		blue		ClampedColorF in value
	param		alpha		ClampedColorF in value
	category	framebuf
	version		1.0
	glxropcode	130
	glfoffset	14
	glfpage		6
	glsoffset	8
	glspage		11
	offset		206

ClearStencil(s)
	return		void
	param		s		StencilValue in value
	category	framebuf
	version		1.0
	glxropcode	131
	glfoffset	15
	glfpage		6
	glsoffset	9
	glspage		11
	offset		207

ClearDepth(depth)
	return		void
	param		depth		ClampedFloat64 in value
	category	framebuf
	version		1.0
	glxropcode	132
	glfoffset	16
	glfpage		6
	glsoffset	10
	glspage		11
	offset		208

StencilMask(mask)
	return		void
	param		mask		MaskedStencilValue in value
	category	framebuf
	version		1.0
	glxropcode	133
	glfflags	capture-handcode
	glfoffset	17
	glfpage		6
	glsoffset	11
	glspage		11
	offset		209

ColorMask(red, green, blue, alpha)
	return		void
	param		red		Boolean in value
	param		green		Boolean in value
	param		blue		Boolean in value
	param		alpha		Boolean in value
	category	framebuf
	version		1.0
	glxropcode	134
	glfoffset	18
	glfpage		6
	glsoffset	12
	glspage		11
	offset		210

DepthMask(flag)
	return		void
	param		flag		Boolean in value
	category	framebuf
	version		1.0
	glxropcode	135
	glfoffset	19
	glfpage		6
	glsoffset	13
	glspage		11
	offset		211

IndexMask(mask)
	return		void
	param		mask		MaskedColorIndexValueI in value
	category	framebuf
	version		1.0
	glxropcode	136
	glfflags	capture-handcode
	glfoffset	20
	glfpage		6
	glsoffset	14
	glspage		11
	offset		212

###############################################################################
#
# misc commands
#
###############################################################################

Accum(op, value)
	return		void
	param		op		AccumOp in value
	param		value		CoordF in value
	category	misc
	version		1.0
	glxropcode	137
	glfoffset	21
	glfpage		6
	glsoffset	15
	glspage		11
	offset		213

Disable(cap)
	return		void
	param		cap		EnableCap in value
	category	misc
	version		1.0
	dlflags		handcode
	glxflags	client-handcode client-intercept
	glxropcode	138
	glfoffset	22
	glfpage		6
	glsflags	client-state
	glsoffset	0
	glspage		12
	offset		214

Enable(cap)
	return		void
	param		cap		EnableCap in value
	category	misc
	version		1.0
	dlflags		handcode
	glxflags	client-handcode client-intercept
	glxropcode	139
	glfoffset	23
	glfpage		6
	glsflags	client-state
	glsoffset	1
	glspage		12
	offset		215

Finish()
	return		void
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	category	misc
	version		1.0
	glxsingle	108
	glfoffset	24
	glfpage		6
	glsoffset	2
	glspage		12
	offset		216

Flush()
	return		void
	dlflags		notlistable
	glxflags	client-handcode server-handcode client-intercept
	category	misc
	version		1.0
	glxsingle	142
	glfoffset	25
	glfpage		6
	glsoffset	3
	glspage		12
	offset		217

PopAttrib()
	return		void
	category	misc
	version		1.0
	glxropcode	141
	glfoffset	26
	glfpage		6
	glsoffset	4
	glspage		12
	offset		218

PushAttrib(mask)
	return		void
	param		mask		AttribMask in value
	category	misc
	version		1.0
	glxropcode	142
	glfoffset	27
	glfpage		6
	glsoffset	5
	glspage		12
	offset		219

###############################################################################
#
# modeling commands
#
###############################################################################

Map1d(target, u1, u2, stride, order, points)
	return		void
	param		target		MapTarget in value
	param		u1		CoordD in value
	param		u2		CoordD in value
	param		stride		Int32 in value
	param		order		CheckedInt32 in value
	param		points		CoordD in array [COMPSIZE(target/stride/order)]
	category	modeling
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	143
	glfflags	capture-handcode
	glfoffset	28
	glfpage		6
	glsflags	capture-handcode
	glsoffset	6
	glspage		12
	offset		220

Map1f(target, u1, u2, stride, order, points)
	return		void
	param		target		MapTarget in value
	param		u1		CoordF in value
	param		u2		CoordF in value
	param		stride		Int32 in value
	param		order		CheckedInt32 in value
	param		points		CoordF in array [COMPSIZE(target/stride/order)]
	category	modeling
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	144
	glfflags	capture-handcode
	glfoffset	29
	glfpage		6
	glsflags	capture-handcode
	glsoffset	7
	glspage		12
	offset		221

Map2d(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points)
	return		void
	param		target		MapTarget in value
	param		u1		CoordD in value
	param		u2		CoordD in value
	param		ustride		Int32 in value
	param		uorder		CheckedInt32 in value
	param		v1		CoordD in value
	param		v2		CoordD in value
	param		vstride		Int32 in value
	param		vorder		CheckedInt32 in value
	param		points		CoordD in array [COMPSIZE(target/ustride/uorder/vstride/vorder)]
	category	modeling
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	145
	glfflags	capture-handcode
	glfoffset	30
	glfpage		6
	glsflags	capture-handcode
	glsoffset	8
	glspage		12
	offset		222

Map2f(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points)
	return		void
	param		target		MapTarget in value
	param		u1		CoordF in value
	param		u2		CoordF in value
	param		ustride		Int32 in value
	param		uorder		CheckedInt32 in value
	param		v1		CoordF in value
	param		v2		CoordF in value
	param		vstride		Int32 in value
	param		vorder		CheckedInt32 in value
	param		points		CoordF in array [COMPSIZE(target/ustride/uorder/vstride/vorder)]
	category	modeling
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	146
	glfflags	capture-handcode
	glfoffset	31
	glfpage		6
	glsflags	capture-handcode
	glsoffset	9
	glspage		12
	offset		223

MapGrid1d(un, u1, u2)
	return		void
	param		un		Int32 in value
	param		u1		CoordD in value
	param		u2		CoordD in value
	category	modeling
	version		1.0
	glxropcode	147
	glfoffset	32
	glfpage		6
	glsoffset	10
	glspage		12
	offset		224

MapGrid1f(un, u1, u2)
	return		void
	param		un		Int32 in value
	param		u1		CoordF in value
	param		u2		CoordF in value
	category	modeling
	version		1.0
	glxropcode	148
	glfoffset	33
	glfpage		6
	glsoffset	11
	glspage		12
	offset		225

MapGrid2d(un, u1, u2, vn, v1, v2)
	return		void
	param		un		Int32 in value
	param		u1		CoordD in value
	param		u2		CoordD in value
	param		vn		Int32 in value
	param		v1		CoordD in value
	param		v2		CoordD in value
	category	modeling
	version		1.0
	glxropcode	149
	glfoffset	34
	glfpage		6
	glsoffset	12
	glspage		12
	offset		226

MapGrid2f(un, u1, u2, vn, v1, v2)
	return		void
	param		un		Int32 in value
	param		u1		CoordF in value
	param		u2		CoordF in value
	param		vn		Int32 in value
	param		v1		CoordF in value
	param		v2		CoordF in value
	category	modeling
	version		1.0
	glxropcode	150
	glfoffset	35
	glfpage		6
	glsoffset	13
	glspage		12
	offset		227

EvalCoord1d(u)
	return		void
	param		u		CoordD in value
	category	modeling
	vectorequiv	EvalCoord1dv
	version		1.0
	glfoffset	36
	glfpage		6
	offset		228

EvalCoord1dv(u)
	return		void
	param		u		CoordD in array [1]
	category	modeling
	version		1.0
	glxropcode	151
	glfoffset	37
	glfpage		6
	glsoffset	14
	glspage		12
	offset		229

EvalCoord1f(u)
	return		void
	param		u		CoordF in value
	category	modeling
	vectorequiv	EvalCoord1fv
	version		1.0
	glfoffset	38
	glfpage		6
	offset		230

EvalCoord1fv(u)
	return		void
	param		u		CoordF in array [1]
	category	modeling
	version		1.0
	glxropcode	152
	glfoffset	39
	glfpage		6
	glsoffset	15
	glspage		12
	offset		231

EvalCoord2d(u, v)
	return		void
	param		u		CoordD in value
	param		v		CoordD in value
	category	modeling
	vectorequiv	EvalCoord2dv
	version		1.0
	glfoffset	40
	glfpage		6
	offset		232

EvalCoord2dv(u)
	return		void
	param		u		CoordD in array [2]
	category	modeling
	version		1.0
	glxropcode	153
	glfoffset	41
	glfpage		6
	glsoffset	0
	glspage		13
	offset		233

EvalCoord2f(u, v)
	return		void
	param		u		CoordF in value
	param		v		CoordF in value
	category	modeling
	vectorequiv	EvalCoord2fv
	version		1.0
	glfoffset	42
	glfpage		6
	offset		234

EvalCoord2fv(u)
	return		void
	param		u		CoordF in array [2]
	category	modeling
	version		1.0
	glxropcode	154
	glfoffset	43
	glfpage		6
	glsoffset	1
	glspage		13
	offset		235

EvalMesh1(mode, i1, i2)
	return		void
	param		mode		MeshMode1 in value
	param		i1		CheckedInt32 in value
	param		i2		CheckedInt32 in value
	category	modeling
	version		1.0
	glxropcode	155
	glfoffset	44
	glfpage		6
	glsoffset	2
	glspage		13
	offset		236

EvalPoint1(i)
	return		void
	param		i		Int32 in value
	category	modeling
	version		1.0
	glxropcode	156
	glfoffset	45
	glfpage		6
	glsoffset	3
	glspage		13
	offset		237

EvalMesh2(mode, i1, i2, j1, j2)
	return		void
	param		mode		MeshMode2 in value
	param		i1		CheckedInt32 in value
	param		i2		CheckedInt32 in value
	param		j1		CheckedInt32 in value
	param		j2		CheckedInt32 in value
	category	modeling
	version		1.0
	glxropcode	157
	glfoffset	46
	glfpage		6
	glsoffset	4
	glspage		13
	offset		238

EvalPoint2(i, j)
	return		void
	param		i		CheckedInt32 in value
	param		j		CheckedInt32 in value
	category	modeling
	version		1.0
	glxropcode	158
	glfoffset	47
	glfpage		6
	glsoffset	5
	glspage		13
	offset		239

###############################################################################
#
# pixel-op commands
#
###############################################################################

AlphaFunc(func, ref)
	return		void
	param		func		AlphaFunction in value
	param		ref		ClampedFloat32 in value
	category	pixel-op
	version		1.0
	glxropcode	159
	glfoffset	48
	glfpage		6
	glsoffset	6
	glspage		13
	offset		240

BlendFunc(sfactor, dfactor)
	return		void
	param		sfactor		BlendingFactorSrc in value
	param		dfactor		BlendingFactorDest in value
	category	pixel-op
	version		1.0
	glxropcode	160
	glfoffset	49
	glfpage		6
	glsoffset	7
	glspage		13
	offset		241

LogicOp(opcode)
	return		void
	param		opcode		LogicOp in value
	category	pixel-op
	version		1.0
	glxropcode	161
	glfoffset	50
	glfpage		6
	glsoffset	8
	glspage		13
	offset		242

StencilFunc(func, ref, mask)
	return		void
	param		func		StencilFunction in value
	param		ref		ClampedStencilValue in value
	param		mask		MaskedStencilValue in value
	category	pixel-op
	version		1.0
	glxropcode	162
	glfflags	capture-handcode
	glfoffset	51
	glfpage		6
	glsoffset	9
	glspage		13
	offset		243

StencilOp(fail, zfail, zpass)
	return		void
	param		fail		StencilOp in value
	param		zfail		StencilOp in value
	param		zpass		StencilOp in value
	category	pixel-op
	version		1.0
	glxropcode	163
	glfoffset	52
	glfpage		6
	glsoffset	10
	glspage		13
	offset		244

DepthFunc(func)
	return		void
	param		func		DepthFunction in value
	category	pixel-op
	version		1.0
	glxropcode	164
	glfoffset	53
	glfpage		6
	glsoffset	11
	glspage		13
	offset		245

###############################################################################
#
# pixel-rw commands
#
###############################################################################

PixelZoom(xfactor, yfactor)
	return		void
	param		xfactor		Float32 in value
	param		yfactor		Float32 in value
	category	pixel-rw
	version		1.0
	glxropcode	165
	glfoffset	54
	glfpage		6
	glsoffset	12
	glspage		13
	offset		246

PixelTransferf(pname, param)
	return		void
	param		pname		PixelTransferParameter in value
	param		param		CheckedFloat32 in value
	category	pixel-rw
	version		1.0
	glxropcode	166
	glfflags	gl-enum
	glfoffset	55
	glfpage		6
	glsflags	gl-enum
	glsoffset	13
	glspage		13
	offset		247

PixelTransferi(pname, param)
	return		void
	param		pname		PixelTransferParameter in value
	param		param		CheckedInt32 in value
	category	pixel-rw
	version		1.0
	glxropcode	167
	glfflags	gl-enum
	glfoffset	56
	glfpage		6
	glsflags	gl-enum
	glsoffset	14
	glspage		13
	offset		248

PixelStoref(pname, param)
	return		void
	param		pname		PixelStoreParameter in value
	param		param		CheckedFloat32 in value
	dlflags		notlistable
	glxflags	client-handcode
	category	pixel-rw
	version		1.0
	glxsingle	109
	glfflags	capture-execute gl-enum
	glfoffset	57
	glfpage		6
	glsflags	client-state gl-enum
	glsoffset	15
	glspage		13
	offset		249

PixelStorei(pname, param)
	return		void
	param		pname		PixelStoreParameter in value
	param		param		CheckedInt32 in value
	dlflags		notlistable
	glxflags	client-handcode
	category	pixel-rw
	version		1.0
	glxsingle	110
	glfflags	capture-execute gl-enum
	glfoffset	58
	glfpage		6
	glsflags	client-state gl-enum
	glsoffset	0
	glspage		14
	offset		250

PixelMapfv(map, mapsize, values)
	return		void
	param		map		PixelMap in value
	param		mapsize		CheckedInt32 in value
	param		values		Float32 in array [mapsize]
	category	pixel-rw
	glxflags	client-handcode
	version		1.0
	glxropcode	168
	glfoffset	59
	glfpage		6
	glsoffset	1
	glspage		14
	offset		251

PixelMapuiv(map, mapsize, values)
	return		void
	param		map		PixelMap in value
	param		mapsize		CheckedInt32 in value
	param		values		UInt32 in array [mapsize]
	category	pixel-rw
	glxflags	client-handcode
	version		1.0
	glxropcode	169
	glfoffset	60
	glfpage		6
	glsoffset	2
	glspage		14
	offset		252

PixelMapusv(map, mapsize, values)
	return		void
	param		map		PixelMap in value
	param		mapsize		CheckedInt32 in value
	param		values		UInt16 in array [mapsize]
	category	pixel-rw
	glxflags	client-handcode
	version		1.0
	glxropcode	170
	glfoffset	61
	glfpage		6
	glsoffset	3
	glspage		14
	offset		253

ReadBuffer(mode)
	return		void
	param		mode		ReadBufferMode in value
	category	pixel-rw
	version		1.0
	glxropcode	171
	glfoffset	62
	glfpage		6
	glsoffset	4
	glspage		14
	offset		254

CopyPixels(x, y, width, height, type)
	return		void
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		type		PixelCopyType in value
	category	pixel-rw
	version		1.0
	glxropcode	172
	glfoffset	63
	glfpage		6
	glsoffset	5
	glspage		14
	offset		255

ReadPixels(x, y, width, height, format, type, pixels)
	return		void
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void out array [COMPSIZE(format/type/width/height)]
	category	pixel-rw
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.0
	glxsingle	111
	glfflags	capture-execute pixel-pack
	glfoffset	0
	glfpage		7
	glsflags	get pixel-pack
	glsoffset	6
	glspage		14
	offset		256

DrawPixels(width, height, format, type, pixels)
	return		void
	param		width		SizeI in value
	param		height		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width/height)]
	category	pixel-rw
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.0
	glxropcode	173
	glfflags	pixel-unpack
	glfoffset	1
	glfpage		7
	glsflags	pixel-unpack
	glsoffset	7
	glspage		14
	offset		257

###############################################################################
#
# state-req commands
#
###############################################################################

GetBooleanv(pname, params)
	return		void
	param		pname		GetPName in value
	param		params		Boolean out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode
	version		1.0
	glxsingle	112
	glfflags	capture-execute
	glfoffset	2
	glfpage		7
	glsflags	client-get get
	glsoffset	8
	glspage		14
	offset		258

GetClipPlane(plane, equation)
	return		void
	param		plane		ClipPlaneName in value
	param		equation	Float64 out array [4]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	113
	glxflags	client-handcode server-handcode
	glfflags	capture-execute
	glfoffset	3
	glfpage		7
	glsflags	get
	glsoffset	9
	glspage		14
	offset		259

GetDoublev(pname, params)
	return		void
	param		pname		GetPName in value
	param		params		Float64 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode
	version		1.0
	glxsingle	114
	glfflags	capture-execute gl-enum
	glfoffset	4
	glfpage		7
	glsflags	client-get get
	glsoffset	10
	glspage		14
	offset		260

GetError()
	return		ErrorCode
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode
	version		1.0
	glxsingle	115
	glfflags	capture-execute store-return
	glfoffset	5
	glfpage		7
	glsflags	get
	glsoffset	11
	glspage		14
	offset		261

GetFloatv(pname, params)
	return		void
	param		pname		GetPName in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode
	version		1.0
	glxsingle	116
	glfflags	capture-execute gl-enum
	glfoffset	6
	glfpage		7
	glsflags	client-get get
	glsoffset	12
	glspage		14
	offset		262

GetIntegerv(pname, params)
	return		void
	param		pname		GetPName in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode
	version		1.0
	glxsingle	117
	glfflags	capture-execute capture-handcode gl-enum
	glfoffset	7
	glfpage		7
	glsflags	client-get get
	glsoffset	13
	glspage		14
	offset		263

GetLightfv(light, pname, params)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	118
	glfflags	capture-execute
	glfoffset	8
	glfpage		7
	glsflags	get
	glsoffset	14
	glspage		14
	offset		264

GetLightiv(light, pname, params)
	return		void
	param		light		LightName in value
	param		pname		LightParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	119
	glfflags	capture-execute
	glfoffset	9
	glfpage		7
	glsflags	get
	glsoffset	15
	glspage		14
	offset		265

GetMapdv(target, query, v)
	return		void
	param		target		MapTarget in value
	param		query		GetMapQuery in value
	param		v		Float64 out array [COMPSIZE(target/query)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	120
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	10
	glfpage		7
	glsflags	get
	glsoffset	0
	glspage		15
	offset		266

GetMapfv(target, query, v)
	return		void
	param		target		MapTarget in value
	param		query		GetMapQuery in value
	param		v		Float32 out array [COMPSIZE(target/query)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	121
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	11
	glfpage		7
	glsflags	get
	glsoffset	1
	glspage		15
	offset		267

GetMapiv(target, query, v)
	return		void
	param		target		MapTarget in value
	param		query		GetMapQuery in value
	param		v		Int32 out array [COMPSIZE(target/query)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	122
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	12
	glfpage		7
	glsflags	get
	glsoffset	2
	glspage		15
	offset		268

GetMaterialfv(face, pname, params)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	123
	glfflags	capture-execute
	glfoffset	13
	glfpage		7
	glsflags	get
	glsoffset	3
	glspage		15
	offset		269

GetMaterialiv(face, pname, params)
	return		void
	param		face		MaterialFace in value
	param		pname		MaterialParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	124
	glfflags	capture-execute
	glfoffset	14
	glfpage		7
	glsflags	get
	glsoffset	4
	glspage		15
	offset		270

GetPixelMapfv(map, values)
	return		void
	param		map		PixelMap in value
	param		values		Float32 out array [COMPSIZE(map)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	125
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	15
	glfpage		7
	glsflags	get
	glsoffset	5
	glspage		15
	offset		271

GetPixelMapuiv(map, values)
	return		void
	param		map		PixelMap in value
	param		values		UInt32 out array [COMPSIZE(map)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	126
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	16
	glfpage		7
	glsflags	get
	glsoffset	6
	glspage		15
	offset		272

GetPixelMapusv(map, values)
	return		void
	param		map		PixelMap in value
	param		values		UInt16 out array [COMPSIZE(map)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	127
	glfflags	capture-execute capture-handcode decode-handcode
	glfoffset	17
	glfpage		7
	glsflags	get
	glsoffset	7
	glspage		15
	offset		273

GetPolygonStipple(mask)
	return		void
	param		mask		UInt8 out array [COMPSIZE()]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.0
	glxsingle	128
	glfflags	capture-execute pixel-pack
	glfoffset	18
	glfpage		7
	glsflags	get pixel-pack
	glsoffset	8
	glspage		15
	offset		274

GetString(name)
	return		String
	param		name		StringName in value
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.0
	glxsingle	129
	glfflags	capture-execute capture-handcode decode-handcode store-return
	glfoffset	19
	glfpage		7
	glsflags	get
	glsoffset	9
	glspage		15
	offset		275

GetTexEnvfv(target, pname, params)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	130
	glfflags	capture-execute gl-enum
	glfoffset	20
	glfpage		7
	glsflags	get
	glsoffset	10
	glspage		15
	offset		276

GetTexEnviv(target, pname, params)
	return		void
	param		target		TextureEnvTarget in value
	param		pname		TextureEnvParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	131
	glfflags	capture-execute gl-enum
	glfoffset	21
	glfpage		7
	glsflags	get
	glsoffset	11
	glspage		15
	offset		277

GetTexGendv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		Float64 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	132
	glfflags	capture-execute gl-enum
	glfoffset	22
	glfpage		7
	glsflags	get
	glsoffset	12
	glspage		15
	offset		278

GetTexGenfv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	133
	glfflags	capture-execute gl-enum
	glfoffset	23
	glfpage		7
	glsflags	get
	glsoffset	13
	glspage		15
	offset		279

GetTexGeniv(coord, pname, params)
	return		void
	param		coord		TextureCoordName in value
	param		pname		TextureGenParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	134
	glfflags	capture-execute gl-enum
	glfoffset	24
	glfpage		7
	glsflags	get
	glsoffset	14
	glspage		15
	offset		280

GetTexImage(target, level, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void out array [COMPSIZE(target/level/format/type)]
	category	state-req
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.0
	glxsingle	135
	glfflags	capture-execute capture-handcode decode-handcode pixel-pack
	glfoffset	25
	glfpage		7
	glsflags	get pixel-pack
	glsoffset	15
	glspage		15
	offset		281

GetTexParameterfv(target, pname, params)
	return		void
	param		target		TextureTarget in value
	param		pname		GetTextureParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	136
	glfflags	capture-execute gl-enum
	glfoffset	26
	glfpage		7
	glsflags	get
	glsoffset	0
	glspage		16
	offset		282

GetTexParameteriv(target, pname, params)
	return		void
	param		target		TextureTarget in value
	param		pname		GetTextureParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	137
	glfflags	capture-execute gl-enum
	glfoffset	27
	glfpage		7
	glsflags	get
	glsoffset	1
	glspage		16
	offset		283

GetTexLevelParameterfv(target, level, pname, params)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		pname		GetTextureParameter in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	138
	glfflags	capture-execute gl-enum
	glfoffset	28
	glfpage		7
	glsflags	get
	glsoffset	2
	glspage		16
	offset		284

GetTexLevelParameteriv(target, level, pname, params)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		pname		GetTextureParameter in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	139
	glfflags	capture-execute gl-enum
	glfoffset	29
	glfpage		7
	glsflags	get
	glsoffset	3
	glspage		16
	offset		285

IsEnabled(cap)
	return		Boolean
	param		cap		EnableCap in value
	category	state-req
	dlflags		notlistable
	version		1.0
	glxflags	client-handcode
	glxsingle	140
	glfflags	capture-execute store-return
	glfoffset	30
	glfpage		7
	glsflags	client-get get
	glsoffset	4
	glspage		16
	offset		286

IsList(list)
	return		Boolean
	param		list		List in value
	category	state-req
	dlflags		notlistable
	version		1.0
	glxsingle	141
	glfflags	capture-execute store-return
	glfoffset	31
	glfpage		7
	glsflags	get
	glsoffset	5
	glspage		16
	offset		287

###############################################################################
#
# xform commands
#
###############################################################################

DepthRange(zNear, zFar)
	return		void
	param		zNear		ClampedFloat64 in value
	param		zFar		ClampedFloat64 in value
	category	xform
	version		1.0
	glxropcode	174
	glfoffset	32
	glfpage		7
	glsoffset	6
	glspage		16
	offset		288

Frustum(left, right, bottom, top, zNear, zFar)
	return		void
	param		left		Float64 in value
	param		right		Float64 in value
	param		bottom		Float64 in value
	param		top		Float64 in value
	param		zNear		Float64 in value
	param		zFar		Float64 in value
	category	xform
	version		1.0
	glxropcode	175
	glfoffset	33
	glfpage		7
	glsoffset	7
	glspage		16
	offset		289

LoadIdentity()
	return		void
	category	xform
	version		1.0
	glxropcode	176
	glfoffset	34
	glfpage		7
	glsoffset	8
	glspage		16
	offset		290

LoadMatrixf(m)
	return		void
	param		m		Float32 in array [16]
	category	xform
	version		1.0
	glxropcode	177
	glfflags	matrix
	glfoffset	35
	glfpage		7
	glsflags	matrix
	glsoffset	9
	glspage		16
	offset		291

LoadMatrixd(m)
	return		void
	param		m		Float64 in array [16]
	category	xform
	version		1.0
	glxropcode	178
	glfflags	matrix
	glfoffset	36
	glfpage		7
	glsflags	matrix
	glsoffset	10
	glspage		16
	offset		292

MatrixMode(mode)
	return		void
	param		mode		MatrixMode in value
	category	xform
	version		1.0
	glxropcode	179
	glfoffset	37
	glfpage		7
	glsoffset	11
	glspage		16
	offset		293

MultMatrixf(m)
	return		void
	param		m		Float32 in array [16]
	category	xform
	version		1.0
	glxropcode	180
	glfflags	matrix
	glfoffset	38
	glfpage		7
	glsflags	matrix
	glsoffset	12
	glspage		16
	offset		294

MultMatrixd(m)
	return		void
	param		m		Float64 in array [16]
	category	xform
	version		1.0
	glxropcode	181
	glfflags	matrix
	glfoffset	39
	glfpage		7
	glsflags	matrix
	glsoffset	13
	glspage		16
	offset		295

Ortho(left, right, bottom, top, zNear, zFar)
	return		void
	param		left		Float64 in value
	param		right		Float64 in value
	param		bottom		Float64 in value
	param		top		Float64 in value
	param		zNear		Float64 in value
	param		zFar		Float64 in value
	category	xform
	version		1.0
	glxropcode	182
	glfoffset	40
	glfpage		7
	glsoffset	14
	glspage		16
	offset		296

PopMatrix()
	return		void
	category	xform
	version		1.0
	glxropcode	183
	glfoffset	41
	glfpage		7
	glsoffset	15
	glspage		16
	offset		297

PushMatrix()
	return		void
	category	xform
	version		1.0
	glxropcode	184
	glfoffset	42
	glfpage		7
	glsoffset	0
	glspage		17
	offset		298

Rotated(angle, x, y, z)
	return		void
	param		angle		Float64 in value
	param		x		Float64 in value
	param		y		Float64 in value
	param		z		Float64 in value
	category	xform
	version		1.0
	glxropcode	185
	glfoffset	43
	glfpage		7
	glsoffset	1
	glspage		17
	offset		299

Rotatef(angle, x, y, z)
	return		void
	param		angle		Float32 in value
	param		x		Float32 in value
	param		y		Float32 in value
	param		z		Float32 in value
	category	xform
	version		1.0
	glxropcode	186
	glfoffset	44
	glfpage		7
	glsoffset	2
	glspage		17
	offset		300

Scaled(x, y, z)
	return		void
	param		x		Float64 in value
	param		y		Float64 in value
	param		z		Float64 in value
	category	xform
	version		1.0
	glxropcode	187
	glfoffset	45
	glfpage		7
	glsoffset	3
	glspage		17
	offset		301

Scalef(x, y, z)
	return		void
	param		x		Float32 in value
	param		y		Float32 in value
	param		z		Float32 in value
	category	xform
	version		1.0
	glxropcode	188
	glfoffset	46
	glfpage		7
	glsoffset	4
	glspage		17
	offset		302

Translated(x, y, z)
	return		void
	param		x		Float64 in value
	param		y		Float64 in value
	param		z		Float64 in value
	category	xform
	version		1.0
	glxropcode	189
	glfoffset	47
	glfpage		7
	glsoffset	5
	glspage		17
	offset		303

Translatef(x, y, z)
	return		void
	param		x		Float32 in value
	param		y		Float32 in value
	param		z		Float32 in value
	category	xform
	version		1.0
	glxropcode	190
	glfoffset	48
	glfpage		7
	glsoffset	6
	glspage		17
	offset		304

Viewport(x, y, width, height)
	return		void
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	category	xform
	version		1.0
	glxropcode	191
	glfoffset	49
	glfpage		7
	glsoffset	7
	glspage		17
	offset		305

###############################################################################
#
# OpenGL 1.1 commands
#
###############################################################################

ArrayElement(i)
	return		void
	param		i		Int32 in value
	category	1_1
	dlflags		handcode
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsoffset	8
	glspage		17
	offset		306

ColorPointer(size, type, stride, pointer)
	return		void
	param		size		Int32 in value
	param		type		ColorPointerType in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	9
	glspage		17
	offset		308

DisableClientState(array)
	return		void
	param		array		EnableCap in value
	category	1_1
	version		1.1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	10
	glspage		17
	offset		309

DrawArrays(mode, first, count)
	return		void
	param		mode		BeginMode in value
	param		first		Int32 in value
	param		count		SizeI in value
	category	1_1
	dlflags		handcode
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glxropcode	193
	glfoffset	0
	glfpage		0
	glsoffset	11
	glspage		17
	offset		310

DrawElements(mode, count, type, indices)
	return		void
	param		mode		BeginMode in value
	param		count		SizeI in value
	param		type		DrawElementsType in value
	param		indices		Void in array [COMPSIZE(count/type)]
	category	1_1
	dlflags		handcode
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsoffset	12
	glspage		17
	offset		311

EdgeFlagPointer(stride, pointer)
	return		void
	param		stride		SizeI in value
	param		pointer		Boolean in array [COMPSIZE(stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	13
	glspage		17
	offset		312

EnableClientState(array)
	return		void
	param		array		EnableCap in value
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	14
	glspage		17
	offset		313

GetPointerv(pname, params)
	return		void
	param		pname		GetPointervPName in value
	param		params		VoidPointer out array [1]
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-get get
	glsoffset	15
	glspage		17
	offset		329

IndexPointer(type, stride, pointer)
	return		void
	param		type		IndexPointerType in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(type/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	0
	glspage		18
	offset		314

InterleavedArrays(format, stride, pointer)
	return		void
	param		format		InterleavedArrayFormat in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(format/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	1
	glspage		18
	offset		317

NormalPointer(type, stride, pointer)
	return		void
	param		type		NormalPointerType in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(type/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	2
	glspage		18
	offset		318

TexCoordPointer(size, type, stride, pointer)
	return		void
	param		size		Int32 in value
	param		type		TexCoordPointerType in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	3
	glspage		18
	offset		320

VertexPointer(size, type, stride, pointer)
	return		void
	param		size		Int32 in value
	param		type		VertexPointerType in value
	param		stride		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride)] retained
	category	1_1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	version		1.1
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	4
	glspage		18
	offset		321

PolygonOffset(factor, units)
	return		void
	param		factor		Float32 in value
	param		units		Float32 in value
	category	1_1
	version		1.1
	glxropcode	192
	glfoffset	0
	glfpage		0
	glsoffset	5
	glspage		18
	offset		319

CopyTexImage1D(target, level, internalformat, x, y, width, border)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		internalformat	PixelInternalFormat in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		border		CheckedInt32 in value
	category	1_1
	version		1.1
	glxropcode	4119
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	6
	glspage		18
	offset		323

CopyTexImage2D(target, level, internalformat, x, y, width, height, border)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		internalformat	PixelInternalFormat in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		border		CheckedInt32 in value
	category	1_1
	version		1.1
	glxropcode	4120
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	7
	glspage		18
	offset		324

CopyTexSubImage1D(target, level, xoffset, x, y, width)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	category	1_1
	version		1.1
	glxropcode	4121
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	8
	glspage		18
	offset		325

CopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		yoffset		CheckedInt32 in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	category	1_1
	version		1.1
	glxropcode	4122
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	9
	glspage		18
	offset		326

TexSubImage1D(target, level, xoffset, width, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		width		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width)]
	category	1_1
	dlflags		handcode
	glxflags	EXT client-handcode server-handcode
	version		1.1
	glxropcode	4099
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	pixel-unpack
	glsoffset	10
	glspage		18
	offset		332

TexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		yoffset		CheckedInt32 in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width/height)]
	category	1_1
	dlflags		handcode
	glxflags	EXT client-handcode server-handcode
	version		1.1
	glxropcode	4100
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	pixel-unpack
	glsoffset	11
	glspage		18
	offset		333

AreTexturesResident(n, textures, residences)
	return		Boolean
	param		n		SizeI in value
	param		textures	Texture in array [n]
	param		residences	Boolean out array [n]
	category	1_1
	glxsingle	143
	dlflags		notlistable
	version		1.1
	glfoffset	0
	glfpage		0
	glsflags	get
	glsoffset	12
	glspage		18
	offset		322
#	 glxflags	 EXT
#	 glxvendorpriv	 11

BindTexture(target, texture)
	return		void
	param		target		TextureTarget in value
	param		texture		Texture in value
	category	1_1
	version		1.1
	glxropcode	4117
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	13
	glspage		18
	offset		307

DeleteTextures(n, textures)
	return		void
	param		n		SizeI in value
	param		textures	Texture in array [n]
	category	1_1
	dlflags		notlistable
	version		1.1
	glxsingle	144
	glfoffset	0
	glfpage		0
	glsoffset	14
	glspage		18
	offset		327
#	 glxflags	 EXT
#	 glxvendorpriv	 12

GenTextures(n, textures)
	return		void
	param		n		SizeI in value
	param		textures	Texture out array [n]
	category	1_1
	dlflags		notlistable
	version		1.1
	glxsingle	145
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsoffset	15
	glspage		18
	offset		328
#	 glxflags	 EXT
#	 glxvendorpriv	 13

IsTexture(texture)
	return		Boolean
	param		texture		Texture in value
	category	1_1
	dlflags		notlistable
	version		1.1
	glxsingle	146
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	get
	glsoffset	0
	glspage		19
	offset		330
#	 glxflags	 EXT
#	 glxvendorpriv	 14

PrioritizeTextures(n, textures, priorities)
	return		void
	param		n		SizeI in value
	param		textures	Texture in array [n]
	param		priorities	ClampedFloat32 in array [n]
	category	1_1
	version		1.1
	glxropcode	4118
	glxflags	EXT
	glfoffset	0
	glfpage		0
	glsoffset	1
	glspage		19
	offset		331

Indexub(c)
	return		void
	param		c		ColorIndexValueUB in value
	category	1_1
	vectorequiv	Indexubv
	version		1.1
	glfoffset	0
	glfpage		0
	offset		315

Indexubv(c)
	return		void
	param		c		ColorIndexValueUB in array [1]
	category	1_1
	version		1.1
	glxropcode	194
	glfoffset	0
	glfpage		0
	glsoffset	2
	glspage		19
	offset		316

PopClientAttrib()
	return		void
	category	1_1
	version		1.1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	3
	glspage		19
	offset		334

PushClientAttrib(mask)
	return		void
	param		mask		ClientAttribMask in value
	category	1_1
	version		1.1
	dlflags		notlistable
	glxflags	client-handcode client-intercept server-handcode
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	4
	glspage		19
	offset		335

###############################################################################
#
# OpenGL 1.2 commands
#
###############################################################################

BlendColor(red, green, blue, alpha)
	return		void
	param		red		ClampedColorF in value
	param		green		ClampedColorF in value
	param		blue		ClampedColorF in value
	param		alpha		ClampedColorF in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4096
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		336

BlendEquation(mode)
	return		void
	param		mode		BlendEquationMode in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4097
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		337

DrawRangeElements(mode, start, end, count, type, indices)
	return		void
	param		mode		BeginMode in value
	param		start		UInt32 in value
	param		end		UInt32 in value
	param		count		SizeI in value
	param		type		DrawElementsType in value
	param		indices		Void in array [COMPSIZE(count/type)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode client-intercept server-handcode
	version		1.0
	glfoffset	ignore
	glfpage		ignore
	glsoffset	ignore
	glspage		ignore
	offset		338

################################################################################
#
# color_table extension commands
#
###############################################################################

ColorTable(target, internalformat, width, format, type, table)
	return		void
	param		target		ColorTableTarget in value
	param		internalformat	PixelInternalFormat in value
	param		width		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		table		Void in array [COMPSIZE(format/type/width)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxropcode	2053
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		339

ColorTableParameterfv(target, pname, params)
	return		void
	param		target		ColorTableTarget in value
	param		pname		ColorTableParameterPName in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	2054
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		340

ColorTableParameteriv(target, pname, params)
	return		void
	param		target		ColorTableTarget in value
	param		pname		ColorTableParameterPName in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	2055
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		341

CopyColorTable(target, internalformat, x, y, width)
	return		void
	param		target		ColorTableTarget in value
	param		internalformat	PixelInternalFormat in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	2056
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		342

GetColorTable(target, format, type, table)
	return		void
	param		target		ColorTableTarget in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		table		Void out array [COMPSIZE(target/format/type)]
	category	1_2
	dlflags		notlistable
	glxflags	client-handcode server-handcode
	version		1.2
	glxsingle	147
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 4098
	offset		343

GetColorTableParameterfv(target, pname, params)
	return		void
	param		target		ColorTableTarget in value
	param		pname		GetColorTableParameterPName in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	148
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 4099
	offset		344

GetColorTableParameteriv(target, pname, params)
	return		void
	param		target		ColorTableTarget in value
	param		pname		GetColorTableParameterPName in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	149
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 4100
	offset		345

################################################################################
#
#  color_subtable extension commands
#
###############################################################################

ColorSubTable(target, start, count, format, type, data)
	return		void
	param		target		ColorTableTarget in value
	param		start		SizeI in value
	param		count		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		data		Void in array [COMPSIZE(format/type/count)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode
	version		1.2
	glxropcode	195
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		346

CopyColorSubTable(target, start, x, y, width)
	return		void
	param		target		ColorTableTarget in value
	param		start		SizeI in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	category	1_2
	version		1.2
	glxropcode	196
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		347

################################################################################
#
#  convolution extension commands
#
###############################################################################

ConvolutionFilter1D(target, internalformat, width, format, type, image)
	return		void
	param		target		ConvolutionTarget in value
	param		internalformat	PixelInternalFormat in value
	param		width		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		image		Void in array [COMPSIZE(format/type/width)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxropcode	4101
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		348

ConvolutionFilter2D(target, internalformat, width, height, format, type, image)
	return		void
	param		target		ConvolutionTarget in value
	param		internalformat	PixelInternalFormat in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		image		Void in array [COMPSIZE(format/type/width/height)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxropcode	4102
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		349

ConvolutionParameterf(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		ConvolutionParameter in value
	param		params		CheckedFloat32 in value
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4103
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		350

ConvolutionParameterfv(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		ConvolutionParameter in value
	param		params		CheckedFloat32 in array [COMPSIZE(pname)]
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4104
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		351

ConvolutionParameteri(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		ConvolutionParameter in value
	param		params		CheckedInt32 in value
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4105
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		352

ConvolutionParameteriv(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		ConvolutionParameter in value
	param		params		CheckedInt32 in array [COMPSIZE(pname)]
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4106
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		353

CopyConvolutionFilter1D(target, internalformat, x, y, width)
	return		void
	param		target		ConvolutionTarget in value
	param		internalformat	PixelInternalFormat in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4107
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		354

CopyConvolutionFilter2D(target, internalformat, x, y, width, height)
	return		void
	param		target		ConvolutionTarget in value
	param		internalformat	PixelInternalFormat in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	category	1_2
	glxflags	EXT
	version		1.2
	glxropcode	4108
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		355

GetConvolutionFilter(target, format, type, image)
	return		void
	param		target		ConvolutionTarget in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		image		Void out array [COMPSIZE(target/format/type)]
	category	1_2
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxsingle	150
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 1
	offset		356

GetConvolutionParameterfv(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		GetConvolutionParameterPName in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	151
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 2
	offset		357

GetConvolutionParameteriv(target, pname, params)
	return		void
	param		target		ConvolutionTarget in value
	param		pname		GetConvolutionParameterPName in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	152
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 3
	offset		358

GetSeparableFilter(target, format, type, row, column, span)
	return		void
	param		target		SeparableTarget in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		row		Void out array [COMPSIZE(target/format/type)]
	param		column		Void out array [COMPSIZE(target/format/type)]
	param		span		Void out array [COMPSIZE(target/format/type)]
	category	1_2
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxsingle	153
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 4
	offset		359

SeparableFilter2D(target, internalformat, width, height, format, type, row, column)
	return		void
	param		target		SeparableTarget in value
	param		internalformat	PixelInternalFormat in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		row		Void in array [COMPSIZE(target/format/type/width)]
	param		column		Void in array [COMPSIZE(target/format/type/height)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxropcode	4109
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		360

################################################################################
#
#  histogram extension commands
#
###############################################################################

GetHistogram(target, reset, format, type, values)
	return		void
	param		target		HistogramTarget in value
	param		reset		Boolean in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		values		Void out array [COMPSIZE(target/format/type)]
	category	1_2
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.2
	glxsingle	154
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 5
	offset		361

GetHistogramParameterfv(target, pname, params)
	return		void
	param		target		HistogramTarget in value
	param		pname		GetHistogramParameterPName in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	155
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 6
	offset		362

GetHistogramParameteriv(target, pname, params)
	return		void
	param		target		HistogramTarget in value
	param		pname		GetHistogramParameterPName in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.2
	glxsingle	156
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 7
	offset		363

GetMinmax(target, reset, format, type, values)
	return		void
	param		target		MinmaxTarget in value
	param		reset		Boolean in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		values		Void out array [COMPSIZE(target/format/type)]
	category	1_2
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	glxsingle	157
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 8
	offset		364

GetMinmaxParameterfv(target, pname, params)
	return		void
	param		target		MinmaxTarget in value
	param		pname		GetMinmaxParameterPName in value
	param		params		Float32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.0
	glxsingle	158
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 9
	offset		365

GetMinmaxParameteriv(target, pname, params)
	return		void
	param		target		MinmaxTarget in value
	param		pname		GetMinmaxParameterPName in value
	param		params		Int32 out array [COMPSIZE(pname)]
	category	1_2
	dlflags		notlistable
	glxflags	EXT
	version		1.0
	glxsingle	159
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
#	 glxvendorpriv	 10
	offset		366

Histogram(target, width, internalformat, sink)
	return		void
	param		target		HistogramTarget in value
	param		width		SizeI in value
	param		internalformat	PixelInternalFormat in value
	param		sink		Boolean in value
	category	1_2
	dlflags		handcode
	glxflags	EXT
	version		1.0
	glxropcode	4110
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		367

Minmax(target, internalformat, sink)
	return		void
	param		target		MinmaxTarget in value
	param		internalformat	PixelInternalFormat in value
	param		sink		Boolean in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4111
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		368

ResetHistogram(target)
	return		void
	param		target		HistogramTarget in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4112
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		369

ResetMinmax(target)
	return		void
	param		target		MinmaxTarget in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4113
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		370

################################################################################
#
#  texture3D extension commands
#
###############################################################################

TexImage3D(target, level, internalformat, width, height, depth, border, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		internalformat	PixelInternalFormat in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		depth		SizeI in value
	param		border		CheckedInt32 in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width/height/depth)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.0
	glxropcode	4114
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		371

TexSubImage3D(target, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		yoffset		CheckedInt32 in value
	param		zoffset		CheckedInt32 in value
	param		width		SizeI in value
	param		height		SizeI in value
	param		depth		SizeI in value
	param		format		PixelFormat in value
	param		type		PixelType in value
	param		pixels		Void in array [COMPSIZE(format/type/width/height/depth)]
	category	1_2
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.0
	glxropcode	4115
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		372

################################################################################
#
#   copy_texture extension commands (specific to texture3D)
#
###############################################################################

CopyTexSubImage3D(target, level, xoffset, yoffset, zoffset, x, y, width, height)
	return		void
	param		target		TextureTarget in value
	param		level		CheckedInt32 in value
	param		xoffset		CheckedInt32 in value
	param		yoffset		CheckedInt32 in value
	param		zoffset		CheckedInt32 in value
	param		x		WinCoord in value
	param		y		WinCoord in value
	param		width		SizeI in value
	param		height		SizeI in value
	category	1_2
	glxflags	EXT
	version		1.0
	glxropcode	4123
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		373

################################################################################
#
#  ARB_multitexture commands (ARB extension)
#
###############################################################################

ActiveTextureARB(texture)
	return		void
	param		texture		TextureUnit in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	197
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		374

ClientActiveTextureARB(texture)
	return		void
	param		texture		TextureUnit in value
	category	ARB_multitexture
	dlflags		notlistable
	glxflags	ARB client-handcode client-intercept server-handcode
	version		1.0
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		375

MultiTexCoord1dARB(target, s)
	return		void
	param		target		TextureUnit in value
	param		s		CoordD in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord1dvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		376

MultiTexCoord1dvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordD in array [1]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	198
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		377

MultiTexCoord1fARB(target, s)
	return		void
	param		target		TextureUnit in value
	param		s		CoordF in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord1fvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		378

MultiTexCoord1fvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordF in array [1]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	199
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		379

MultiTexCoord1iARB(target, s)
	return		void
	param		target		TextureUnit in value
	param		s		CoordI in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord1ivARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		380

MultiTexCoord1ivARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordI in array [1]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	200
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		381

MultiTexCoord1sARB(target, s)
	return		void
	param		target		TextureUnit in value
	param		s		CoordS in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord1svARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		382

MultiTexCoord1svARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordS in array [1]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	201
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		383

MultiTexCoord2dARB(target, s, t)
	return		void
	param		target		TextureUnit in value
	param		s		CoordD in value
	param		t		CoordD in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord2dvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		384

MultiTexCoord2dvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordD in array [2]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	202
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		385

MultiTexCoord2fARB(target, s, t)
	return		void
	param		target		TextureUnit in value
	param		s		CoordF in value
	param		t		CoordF in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord2fvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		386

MultiTexCoord2fvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordF in array [2]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	203
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		387

MultiTexCoord2iARB(target, s, t)
	return		void
	param		target		TextureUnit in value
	param		s		CoordI in value
	param		t		CoordI in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord2ivARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		388

MultiTexCoord2ivARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordI in array [2]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	204
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		389

MultiTexCoord2sARB(target, s, t)
	return		void
	param		target		TextureUnit in value
	param		s		CoordS in value
	param		t		CoordS in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord2svARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		390

MultiTexCoord2svARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordS in array [2]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	205
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		391

MultiTexCoord3dARB(target, s, t, r)
	return		void
	param		target		TextureUnit in value
	param		s		CoordD in value
	param		t		CoordD in value
	param		r		CoordD in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord3dvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		392

MultiTexCoord3dvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordD in array [3]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	206
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		393

MultiTexCoord3fARB(target, s, t, r)
	return		void
	param		target		TextureUnit in value
	param		s		CoordF in value
	param		t		CoordF in value
	param		r		CoordF in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord3fvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		394

MultiTexCoord3fvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordF in array [3]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	207
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		395

MultiTexCoord3iARB(target, s, t, r)
	return		void
	param		target		TextureUnit in value
	param		s		CoordI in value
	param		t		CoordI in value
	param		r		CoordI in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord3ivARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		396

MultiTexCoord3ivARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordI in array [3]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	208
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		397

MultiTexCoord3sARB(target, s, t, r)
	return		void
	param		target		TextureUnit in value
	param		s		CoordS in value
	param		t		CoordS in value
	param		r		CoordS in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord3svARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		398

MultiTexCoord3svARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordS in array [3]
	category	ARB_multitexture
	version		1.0
	glxflags	ARB
	glxropcode	209
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		399

MultiTexCoord4dARB(target, s, t, r, q)
	return		void
	param		target		TextureUnit in value
	param		s		CoordD in value
	param		t		CoordD in value
	param		r		CoordD in value
	param		q		CoordD in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord4dvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		400

MultiTexCoord4dvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordD in array [4]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	210
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		401

MultiTexCoord4fARB(target, s, t, r, q)
	return		void
	param		target		TextureUnit in value
	param		s		CoordF in value
	param		t		CoordF in value
	param		r		CoordF in value
	param		q		CoordF in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord4fvARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		402

MultiTexCoord4fvARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordF in array [4]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	211
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		403

MultiTexCoord4iARB(target, s, t, r, q)
	return		void
	param		target		TextureUnit in value
	param		s		CoordI in value
	param		t		CoordI in value
	param		r		CoordI in value
	param		q		CoordI in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord4ivARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		404

MultiTexCoord4ivARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordI in array [4]
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	glxropcode	212
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		405

MultiTexCoord4sARB(target, s, t, r, q)
	return		void
	param		target		TextureUnit in value
	param		s		CoordS in value
	param		t		CoordS in value
	param		r		CoordS in value
	param		q		CoordS in value
	category	ARB_multitexture
	glxflags	ARB
	version		1.0
	vectorequiv	MultiTexCoord4svARB
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		406

MultiTexCoord4svARB(target, v)
	return		void
	param		target		TextureUnit in value
	param		v		CoordS in array [4]
	category	ARB_multitexture
	glxflags	ARB
	version		1.2
	glxropcode	213
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	ignore
	glsoffset	0
	glspage		0
	offset		407

###############################################################################
#
# EXT_compiled_vertex_array extension commands
#
###############################################################################

LockArraysEXT(first, count)
	return		void
	param		first		Int32 in value
	param		count		SizeI in value
	category	EXT_compiled_vertex_array
	version		1.1
	dlflags		notlistable
	extension	soft WINSOFT
	glxflags	ignore
	glfflags	ignore
	glsflags	ignore

UnlockArraysEXT()
	return		void
	category	EXT_compiled_vertex_array
	version		1.1
	dlflags		notlistable
	extension	soft WINSOFT
	glxflags	ignore
	glfflags	ignore
	glsflags	ignore

###############################################################################
#
# EXT_vertex_array extension commands
#
###############################################################################

ArrayElementEXT(i)
	return		void
	param		i		Int32 in value
	category	EXT_vertex_array
	dlflags		handcode
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	capture-handcode ignore
	glfoffset	0
	glfpage		0
	glsoffset	5
	glspage		4093

ColorPointerEXT(size, type, stride, count, pointer)
	return		void
	param		size		Int32 in value
	param		type		ColorPointerType in value
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	6
	glspage		4093

EdgeFlagPointerEXT(stride, count, pointer)
	return		void
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Boolean in array [COMPSIZE(stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	8
	glspage		4093

GetPointervEXT(pname, params)
	return		void
	param		pname		GetPointervPName in value
	param		params		VoidPointer out array [1]
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-get get
	glsoffset	9
	glspage		4093

IndexPointerEXT(type, stride, count, pointer)
	return		void
	param		type		IndexPointerType in value
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Void in array [COMPSIZE(type/stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	10
	glspage		4093

NormalPointerEXT(type, stride, count, pointer)
	return		void
	param		type		NormalPointerType in value
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Void in array [COMPSIZE(type/stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	11
	glspage		4093

TexCoordPointerEXT(size, type, stride, count, pointer)
	return		void
	param		size		Int32 in value
	param		type		TexCoordPointerType in value
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	12
	glspage		4093

VertexPointerEXT(size, type, stride, count, pointer)
	return		void
	param		size		Int32 in value
	param		type		VertexPointerType in value
	param		stride		SizeI in value
	param		count		SizeI in value
	param		pointer		Void in array [COMPSIZE(size/type/stride/count)] retained
	category	EXT_vertex_array
	dlflags		notlistable
	glxflags	client-handcode server-handcode EXT
	version		1.0
	extension	soft WINSOFT
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	client-state
	glsoffset	13
	glspage		4093

###############################################################################
#
# Extensions used instead of core protocol in Xsgi
#
###############################################################################

DrawArraysEXT(mode, first, count)
	return		void
	param		mode		BeginMode in value
	param		first		Int32 in value
	param		count		SizeI in value
	category	1_1
	dlflags		handcode
	glxflags	EXT client-handcode client-intercept server-handcode
	version		1.1
	extension	soft
	glxropcode	4116
	glfoffset	0
	glfpage		0
	glsoffset	11
	glspage		17
	wglflags	ignore

AreTexturesResidentEXT(n, textures, residences)
	return		Boolean
	param		n		SizeI in value
	param		textures	Texture in array [n]
	param		residences	Boolean out array [n]
	category	1_1
	glxvendorpriv	11
	dlflags		notlistable
	glxflags	EXT
	version		1.1
	extension	soft
	glfoffset	0
	glfpage		0
	glsflags	get
	glsoffset	12
	glspage		18
	wglflags	ignore

DeleteTexturesEXT(n, textures)
	return		void
	param		n		SizeI in value
	param		textures	Texture in array [n]
	category	1_1
	dlflags		notlistable
	glxflags	EXT
	version		1.1
	extension	soft
	glxvendorpriv	12
	glfoffset	0
	glfpage		0
	glsoffset	14
	glspage		18
	wglflags	ignore

GenTexturesEXT(n, textures)
	return		void
	param		n		SizeI in value
	param		textures	Texture out array [n]
	category	1_1
	dlflags		notlistable
	glxflags	EXT
	version		1.1
	extension	soft
	glxvendorpriv	13
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsoffset	15
	glspage		18
	wglflags	ignore

IsTextureEXT(texture)
	return		Boolean
	param		texture		Texture in value
	category	1_1
	dlflags		notlistable
	glxflags	EXT
	version		1.1
	extension	soft
	glxvendorpriv	14
	glfflags	ignore
	glfoffset	0
	glfpage		0
	glsflags	get
	glsoffset	0
	glspage		19
	wglflags	ignore
