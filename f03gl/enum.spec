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
# $Header: //depot/main/gfx/include/gl/spec/enum.spec#10 $

###############################################################################
#
# Before modifying this file, read the following:
#
#   Enumerant values for extensions CANNOT be chosen arbitrarily
#   since the enumerant value space is shared by all OpenGL licensees.
#   It is therefore imperative that the procedures described in this
#   file be followed carefully when allocating extension enum values.
#
# - Use tabs, not spaces.
#
# - When adding enum values for a new extension, use existing extensions
#   as a guide.
#
# - SGI maintains a registry (NOT this file) of OpenGL extension enum
#   values for all of the vendors in the OpenGL community. When a vendor
#   has committed to releasing a new extension and needs to allocate enum
#   values for that extension, the vendor may request that SGI allocate
#   a previously unallocated block of 16 enum values, in the range
#   0x8000-0xFFFF, for the vendor's exclusive use.
#
# - The vendor that introduces an extension will allocate enum values for
#   it as if it is a single-vendor extension, even if it is a multi-vendor
#   (EXT) extension.
#
# - The file enum.spec is used to generate <GL/gl.h>. The file enumfuture.spec
#   is used to generate <GL/glfuture.h>, which is conditionally #included in
#   <GL/gl.h>. <GL/glfuture.h> is for extension declarations that are needed
#   for development work but are not yet ready for customers to see in
#   <GL/gl.h>.
#
# - If an IHV hasn't yet committed to releasing an extension, put its enum
#   definitions in enumfuture.spec AND add a comment to enum.spec that
#   contains the name of the extension and the range of enum values used
#   by the extension. When a vendor commits to releasing the extension, move
#   its enum definitions from enumfuture.spec to enum.spec and remove the
#   comment from enum.spec.
#
# - Allocate all of the enum values for an extension in a single contiguous
#   block.
#
# - If an extension is experimental, allocate temporary enum values in the
#   range 0x6000-0x8000 during development work. When you are committed to
#   releasing the extension, allocate permanent enum values (see below).
#   There are two reasons for this policy:
#
#   1. It is desirable to keep extension enum values tightly packed and to
#      make all of the enum values for an extension be contiguous. This is
#      possible only if permanent enum values for a new extension are not
#      allocated until the extension spec is stable and the number of new
#      enum values needed by the extension has therefore stopped changing.
#
#   2. OpenGL ARB policy is that a vendor may allocate a new block of 16
#      extension enum values only if it has committed to releasing an
#      extension that will use values in that block.
#
# - To allocate a new block of permanent enum values for an extension
#   contant Silicon Graphics at opengl-registry@sgi.com in order to
#   reserve 16-entry enum blocks.
#
###############################################################################


######################################################################
# basic opengl
######################################################################

Version			define:
			VERSION_1_1				= 1
			VERSION_1_2				= 1

Extensions		define:
			ARB_imaging				= 1
			ARB_multitexture			= 1

AccumOp			enum:
			ACCUM					= 0x0100
			LOAD					= 0x0101
			RETURN					= 0x0102
			MULT					= 0x0103
			ADD					= 0x0104

AlphaFunction		enum:
			NEVER					= 0x0200
			LESS					= 0x0201
			EQUAL					= 0x0202
			LEQUAL					= 0x0203
			GREATER					= 0x0204
			NOTEQUAL				= 0x0205
			GEQUAL					= 0x0206
			ALWAYS					= 0x0207

AttribMask		enum:
			CURRENT_BIT				= 0x00000001
			POINT_BIT				= 0x00000002
			LINE_BIT				= 0x00000004
			POLYGON_BIT				= 0x00000008
			POLYGON_STIPPLE_BIT			= 0x00000010
			PIXEL_MODE_BIT				= 0x00000020
			LIGHTING_BIT				= 0x00000040
			FOG_BIT					= 0x00000080
			DEPTH_BUFFER_BIT			= 0x00000100
			ACCUM_BUFFER_BIT			= 0x00000200
			STENCIL_BUFFER_BIT			= 0x00000400
			VIEWPORT_BIT				= 0x00000800
			TRANSFORM_BIT				= 0x00001000
			ENABLE_BIT				= 0x00002000
			COLOR_BUFFER_BIT			= 0x00004000
			HINT_BIT				= 0x00008000
			EVAL_BIT				= 0x00010000
			LIST_BIT				= 0x00020000
			TEXTURE_BIT				= 0x00040000
			SCISSOR_BIT				= 0x00080000
			ALL_ATTRIB_BITS				= 0x000fffff

BeginMode		enum:
			POINTS					= 0x0000
			LINES					= 0x0001
			LINE_LOOP				= 0x0002
			LINE_STRIP				= 0x0003
			TRIANGLES				= 0x0004
			TRIANGLE_STRIP				= 0x0005
			TRIANGLE_FAN				= 0x0006
			QUADS					= 0x0007
			QUAD_STRIP				= 0x0008
			POLYGON					= 0x0009

BlendEquationMode	enum:
			use GetTarget LOGIC_OP
			use blend_minmax FUNC_ADD
			use blend_minmax MIN
			use blend_minmax MAX
			use blend_subtract FUNC_SUBTRACT
			use blend_subtract FUNC_REVERSE_SUBTRACT

BlendingFactorDest	enum:
			ZERO					= 0
			ONE					= 1
			SRC_COLOR				= 0x0300
			ONE_MINUS_SRC_COLOR			= 0x0301
			SRC_ALPHA				= 0x0302
			ONE_MINUS_SRC_ALPHA			= 0x0303
			DST_ALPHA				= 0x0304
			ONE_MINUS_DST_ALPHA			= 0x0305
			use blend_color CONSTANT_COLOR
			use blend_color ONE_MINUS_CONSTANT_COLOR
			use blend_color CONSTANT_ALPHA
			use blend_color ONE_MINUS_CONSTANT_ALPHA

BlendingFactorSrc	enum:
			use BlendingFactorDest ZERO
			use BlendingFactorDest ONE
			DST_COLOR				= 0x0306
			ONE_MINUS_DST_COLOR			= 0x0307
			SRC_ALPHA_SATURATE			= 0x0308
			use BlendingFactorDest SRC_ALPHA
			use BlendingFactorDest ONE_MINUS_SRC_ALPHA
			use BlendingFactorDest DST_ALPHA
			use BlendingFactorDest ONE_MINUS_DST_ALPHA
			use blend_color CONSTANT_COLOR
			use blend_color ONE_MINUS_CONSTANT_COLOR
			use blend_color CONSTANT_ALPHA
			use blend_color ONE_MINUS_CONSTANT_ALPHA

Boolean			enum:
			TRUE					= 1
			FALSE					= 0

ClearBufferMask		enum:
			use AttribMask COLOR_BUFFER_BIT
			use AttribMask ACCUM_BUFFER_BIT
			use AttribMask STENCIL_BUFFER_BIT
			use AttribMask DEPTH_BUFFER_BIT

ClientArrayType		enum:
			use vertex_array VERTEX_ARRAY
			use vertex_array NORMAL_ARRAY
			use vertex_array COLOR_ARRAY
			use vertex_array INDEX_ARRAY
			use vertex_array TEXTURE_COORD_ARRAY
			use vertex_array EDGE_FLAG_ARRAY

# 0x3000 through 0x3FFF are reserved for clip planes
ClipPlaneName		enum:
			CLIP_PLANE0				= 0x3000
			CLIP_PLANE1				= 0x3001
			CLIP_PLANE2				= 0x3002
			CLIP_PLANE3				= 0x3003
			CLIP_PLANE4				= 0x3004
			CLIP_PLANE5				= 0x3005

ColorMaterialFace	enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

ColorMaterialParameter	enum:
			use LightParameter AMBIENT
			use LightParameter DIFFUSE
			use LightParameter SPECULAR
			use MaterialParameter EMISSION
			use MaterialParameter AMBIENT_AND_DIFFUSE

ColorPointerType	enum:
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			use DataType DOUBLE

ColorTableParameterPName enum:
			use color_table COLOR_TABLE_SCALE
			use color_table COLOR_TABLE_BIAS

ColorTableTarget	enum:
			use color_table COLOR_TABLE
			use color_table POST_CONVOLUTION_COLOR_TABLE
			use color_table POST_COLOR_MATRIX_COLOR_TABLE
			use color_table PROXY_COLOR_TABLE
			use color_table PROXY_POST_CONVOLUTION_COLOR_TABLE
			use color_table PROXY_POST_COLOR_MATRIX_COLOR_TABLE

ConvolutionBorderMode	enum:
			use convolution REDUCE
			use convolution IGNORE_BORDER
			use convolution CONSTANT_BORDER

ConvolutionParameter	enum:
			use convolution CONVOLUTION_BORDER_MODE
			use convolution CONVOLUTION_FILTER_SCALE
			use convolution CONVOLUTION_FILTER_BIAS

ConvolutionTarget	enum:
			use convolution CONVOLUTION_1D
			use convolution CONVOLUTION_2D

CullFaceMode		enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

DataType		enum:
			BYTE					= 0x1400
			UNSIGNED_BYTE				= 0x1401
			SHORT					= 0x1402
			UNSIGNED_SHORT				= 0x1403
			INT					= 0x1404
			UNSIGNED_INT				= 0x1405
			FLOAT					= 0x1406
			2_BYTES					= 0x1407
			3_BYTES					= 0x1408
			4_BYTES					= 0x1409
			DOUBLE					= 0x140A

DepthFunction		enum:
			use AlphaFunction NEVER
			use AlphaFunction LESS
			use AlphaFunction EQUAL
			use AlphaFunction LEQUAL
			use AlphaFunction GREATER
			use AlphaFunction NOTEQUAL
			use AlphaFunction GEQUAL
			use AlphaFunction ALWAYS

DrawBufferMode		enum:
			NONE					= 0
			FRONT_LEFT				= 0x0400
			FRONT_RIGHT				= 0x0401
			BACK_LEFT				= 0x0402
			BACK_RIGHT				= 0x0403
			FRONT					= 0x0404
			BACK					= 0x0405
			LEFT					= 0x0406
			RIGHT					= 0x0407
			FRONT_AND_BACK				= 0x0408
			AUX0					= 0x0409
			AUX1					= 0x040A
			AUX2					= 0x040B
			AUX3					= 0x040C

Enable			enum:
			use GetTarget FOG
			use GetTarget LIGHTING
			use GetTarget TEXTURE_1D
			use GetTarget TEXTURE_2D
			use GetTarget LINE_STIPPLE
			use GetTarget POLYGON_STIPPLE
			use GetTarget CULL_FACE
			use GetTarget ALPHA_TEST
			use GetTarget BLEND
			use GetTarget INDEX_LOGIC_OP
			use logic_op COLOR_LOGIC_OP
			use GetTarget DITHER
			use GetTarget STENCIL_TEST
			use GetTarget DEPTH_TEST
			use GetTarget CLIP_PLANE0
			use GetTarget CLIP_PLANE1
			use GetTarget CLIP_PLANE2
			use GetTarget CLIP_PLANE3
			use GetTarget CLIP_PLANE4
			use GetTarget CLIP_PLANE5
			use GetTarget LIGHT0
			use GetTarget LIGHT1
			use GetTarget LIGHT2
			use GetTarget LIGHT3
			use GetTarget LIGHT4
			use GetTarget LIGHT5
			use GetTarget LIGHT6
			use GetTarget LIGHT7
			use GetTarget TEXTURE_GEN_S
			use GetTarget TEXTURE_GEN_T
			use GetTarget TEXTURE_GEN_R
			use GetTarget TEXTURE_GEN_Q
			use GetTarget MAP1_VERTEX_3
			use GetTarget MAP1_VERTEX_4
			use GetTarget MAP1_COLOR_4
			use GetTarget MAP1_INDEX
			use GetTarget MAP1_NORMAL
			use GetTarget MAP1_TEXTURE_COORD_1
			use GetTarget MAP1_TEXTURE_COORD_2
			use GetTarget MAP1_TEXTURE_COORD_3
			use GetTarget MAP1_TEXTURE_COORD_4
			use GetTarget MAP2_VERTEX_3
			use GetTarget MAP2_VERTEX_4
			use GetTarget MAP2_COLOR_4
			use GetTarget MAP2_INDEX
			use GetTarget MAP2_NORMAL
			use GetTarget MAP2_TEXTURE_COORD_1
			use GetTarget MAP2_TEXTURE_COORD_2
			use GetTarget MAP2_TEXTURE_COORD_3
			use GetTarget MAP2_TEXTURE_COORD_4
			use GetTarget POINT_SMOOTH
			use GetTarget LINE_SMOOTH
			use GetTarget POLYGON_SMOOTH
			use GetTarget SCISSOR_TEST
			use GetTarget COLOR_MATERIAL
			use GetTarget NORMALIZE
			use GetTarget AUTO_NORMAL

			use vertex_array VERTEX_ARRAY
			use vertex_array NORMAL_ARRAY
			use vertex_array COLOR_ARRAY
			use vertex_array INDEX_ARRAY
			use vertex_array TEXTURE_COORD_ARRAY
			use vertex_array EDGE_FLAG_ARRAY

			use polygon_offset POLYGON_OFFSET_POINT
			use polygon_offset POLYGON_OFFSET_LINE
			use polygon_offset POLYGON_OFFSET_FILL

			use color_table COLOR_TABLE
			use color_table POST_CONVOLUTION_COLOR_TABLE
			use color_table POST_COLOR_MATRIX_COLOR_TABLE

			use convolution CONVOLUTION_1D
			use convolution CONVOLUTION_2D
			use convolution SEPARABLE_2D

			use histogram HISTOGRAM
			use histogram MINMAX

			use rescale_normal RESCALE_NORMAL

			use texture3D TEXTURE_3D

ErrorCode		enum:
			NO_ERROR				= 0
			INVALID_ENUM				= 0x0500
			INVALID_VALUE				= 0x0501
			INVALID_OPERATION			= 0x0502
			STACK_OVERFLOW				= 0x0503
			STACK_UNDERFLOW				= 0x0504
			OUT_OF_MEMORY				= 0x0505
			use histogram TABLE_TOO_LARGE

FeedBackMode		enum:
			2D					= 0x0600
			3D					= 0x0601
			3D_COLOR				= 0x0602
			3D_COLOR_TEXTURE			= 0x0603
			4D_COLOR_TEXTURE			= 0x0604

FeedBackToken		enum:
			PASS_THROUGH_TOKEN			= 0x0700
			POINT_TOKEN				= 0x0701
			LINE_TOKEN				= 0x0702
			POLYGON_TOKEN				= 0x0703
			BITMAP_TOKEN				= 0x0704
			DRAW_PIXEL_TOKEN			= 0x0705
			COPY_PIXEL_TOKEN			= 0x0706
			LINE_RESET_TOKEN			= 0x0707

FogMode			enum:
			use TextureMagFilter LINEAR
			EXP					= 0x0800
			EXP2					= 0x0801

FogParameter		enum:
			use GetTarget FOG_COLOR
			use GetTarget FOG_DENSITY
			use GetTarget FOG_END
			use GetTarget FOG_INDEX
			use GetTarget FOG_MODE
			use GetTarget FOG_START

FrontFaceDirection	enum:
			CW					= 0x0900
			CCW					= 0x0901

GetColorTableParameterPName enum:
			use color_table COLOR_TABLE_SCALE
			use color_table COLOR_TABLE_BIAS
			use color_table COLOR_TABLE_FORMAT
			use color_table COLOR_TABLE_WIDTH
			use color_table COLOR_TABLE_RED_SIZE
			use color_table COLOR_TABLE_GREEN_SIZE
			use color_table COLOR_TABLE_BLUE_SIZE
			use color_table COLOR_TABLE_ALPHA_SIZE
			use color_table COLOR_TABLE_LUMINANCE_SIZE
			use color_table COLOR_TABLE_INTENSITY_SIZE

GetConvolutionParameterPName enum:
			use convolution CONVOLUTION_BORDER_COLOR
			use convolution CONVOLUTION_BORDER_MODE
			use convolution CONVOLUTION_FILTER_SCALE
			use convolution CONVOLUTION_FILTER_BIAS
			use convolution CONVOLUTION_FORMAT
			use convolution CONVOLUTION_WIDTH
			use convolution CONVOLUTION_HEIGHT
			use convolution MAX_CONVOLUTION_WIDTH
			use convolution MAX_CONVOLUTION_HEIGHT

GetHistogramParameterPName enum:
			use histogram HISTOGRAM_WIDTH
			use histogram HISTOGRAM_FORMAT
			use histogram HISTOGRAM_RED_SIZE
			use histogram HISTOGRAM_GREEN_SIZE
			use histogram HISTOGRAM_BLUE_SIZE
			use histogram HISTOGRAM_ALPHA_SIZE
			use histogram HISTOGRAM_LUMINANCE_SIZE
			use histogram HISTOGRAM_SINK

GetMapTarget		enum:
			COEFF					= 0x0A00
			ORDER					= 0x0A01
			DOMAIN					= 0x0A02

GetMinmaxParameterPName enum:
			use histogram MINMAX_FORMAT
			use histogram MINMAX_SINK

GetPixelMap		enum:
			use GetTarget PIXEL_MAP_I_TO_I
			use GetTarget PIXEL_MAP_S_TO_S
			use GetTarget PIXEL_MAP_I_TO_R
			use GetTarget PIXEL_MAP_I_TO_G
			use GetTarget PIXEL_MAP_I_TO_B
			use GetTarget PIXEL_MAP_I_TO_A
			use GetTarget PIXEL_MAP_R_TO_R
			use GetTarget PIXEL_MAP_G_TO_G
			use GetTarget PIXEL_MAP_B_TO_B
			use GetTarget PIXEL_MAP_A_TO_A

GetPointerTarget	enum:
			use vertex_array VERTEX_ARRAY_POINTER
			use vertex_array NORMAL_ARRAY_POINTER
			use vertex_array COLOR_ARRAY_POINTER
			use vertex_array INDEX_ARRAY_POINTER
			use vertex_array TEXTURE_COORD_ARRAY_POINTER
			use vertex_array EDGE_FLAG_ARRAY_POINTER

GetTarget		enum:
			CURRENT_COLOR				= 0x0B00
			CURRENT_INDEX				= 0x0B01
			CURRENT_NORMAL				= 0x0B02
			CURRENT_TEXTURE_COORDS			= 0x0B03
			CURRENT_RASTER_COLOR			= 0x0B04
			CURRENT_RASTER_INDEX			= 0x0B05
			CURRENT_RASTER_TEXTURE_COORDS		= 0x0B06
			CURRENT_RASTER_POSITION			= 0x0B07
			CURRENT_RASTER_POSITION_VALID		= 0x0B08
			CURRENT_RASTER_DISTANCE			= 0x0B09

			POINT_SMOOTH				= 0x0B10
			POINT_SIZE				= 0x0B11
			POINT_SIZE_RANGE			= 0x0B12
			POINT_SIZE_GRANULARITY			= 0x0B13

			LINE_SMOOTH				= 0x0B20
			LINE_WIDTH				= 0x0B21
			LINE_WIDTH_RANGE			= 0x0B22
			LINE_WIDTH_GRANULARITY			= 0x0B23
			LINE_STIPPLE				= 0x0B24
			LINE_STIPPLE_PATTERN			= 0x0B25
			LINE_STIPPLE_REPEAT			= 0x0B26
			use GetTarget1_2 SMOOTH_POINT_SIZE_RANGE
			use GetTarget1_2 SMOOTH_POINT_SIZE_GRANULARITY
			use GetTarget1_2 SMOOTH_LINE_WIDTH_RANGE
			use GetTarget1_2 SMOOTH_LINE_WIDTH_GRANULARITY
			use GetTarget1_2 ALIASED_POINT_SIZE_RANGE
			use GetTarget1_2 ALIASED_LINE_WIDTH_RANGE

			LIST_MODE				= 0x0B30
			MAX_LIST_NESTING			= 0x0B31
			LIST_BASE				= 0x0B32
			LIST_INDEX				= 0x0B33

			POLYGON_MODE				= 0x0B40
			POLYGON_SMOOTH				= 0x0B41
			POLYGON_STIPPLE				= 0x0B42
			EDGE_FLAG				= 0x0B43
			CULL_FACE				= 0x0B44
			CULL_FACE_MODE				= 0x0B45
			FRONT_FACE				= 0x0B46

			LIGHTING				= 0x0B50
			LIGHT_MODEL_LOCAL_VIEWER		= 0x0B51
			LIGHT_MODEL_TWO_SIDE			= 0x0B52
			LIGHT_MODEL_AMBIENT			= 0x0B53
			SHADE_MODEL				= 0x0B54
			COLOR_MATERIAL_FACE			= 0x0B55
			COLOR_MATERIAL_PARAMETER		= 0x0B56
			COLOR_MATERIAL				= 0x0B57

			FOG					= 0x0B60
			FOG_INDEX				= 0x0B61
			FOG_DENSITY				= 0x0B62
			FOG_START				= 0x0B63
			FOG_END					= 0x0B64
			FOG_MODE				= 0x0B65
			FOG_COLOR				= 0x0B66

			DEPTH_RANGE				= 0x0B70
			DEPTH_TEST				= 0x0B71
			DEPTH_WRITEMASK				= 0x0B72
			DEPTH_CLEAR_VALUE			= 0x0B73
			DEPTH_FUNC				= 0x0B74

			ACCUM_CLEAR_VALUE			= 0x0B80

			STENCIL_TEST				= 0x0B90
			STENCIL_CLEAR_VALUE			= 0x0B91
			STENCIL_FUNC				= 0x0B92
			STENCIL_VALUE_MASK			= 0x0B93
			STENCIL_FAIL				= 0x0B94
			STENCIL_PASS_DEPTH_FAIL			= 0x0B95
			STENCIL_PASS_DEPTH_PASS			= 0x0B96
			STENCIL_REF				= 0x0B97
			STENCIL_WRITEMASK			= 0x0B98

			MATRIX_MODE				= 0x0BA0
			NORMALIZE				= 0x0BA1
			VIEWPORT				= 0x0BA2
			MODELVIEW_STACK_DEPTH			= 0x0BA3
			PROJECTION_STACK_DEPTH			= 0x0BA4
			TEXTURE_STACK_DEPTH			= 0x0BA5
			MODELVIEW_MATRIX			= 0x0BA6
			PROJECTION_MATRIX			= 0x0BA7
			TEXTURE_MATRIX				= 0x0BA8

			ATTRIB_STACK_DEPTH			= 0x0BB0
			CLIENT_ATTRIB_STACK_DEPTH		= 0x0BB1

			ALPHA_TEST				= 0x0BC0
			ALPHA_TEST_FUNC				= 0x0BC1
			ALPHA_TEST_REF				= 0x0BC2

			DITHER					= 0x0BD0

			BLEND_DST				= 0x0BE0
			BLEND_SRC				= 0x0BE1
			BLEND					= 0x0BE2

			LOGIC_OP_MODE				= 0x0BF0
			INDEX_LOGIC_OP				= 0x0BF1
			COLOR_LOGIC_OP				= 0x0BF2

			AUX_BUFFERS				= 0x0C00
			DRAW_BUFFER				= 0x0C01
			READ_BUFFER				= 0x0C02

			SCISSOR_BOX				= 0x0C10
			SCISSOR_TEST				= 0x0C11

			INDEX_CLEAR_VALUE			= 0x0C20
			INDEX_WRITEMASK				= 0x0C21
			COLOR_CLEAR_VALUE			= 0x0C22
			COLOR_WRITEMASK				= 0x0C23

			INDEX_MODE				= 0x0C30
			RGBA_MODE				= 0x0C31
			DOUBLEBUFFER				= 0x0C32
			STEREO					= 0x0C33

			RENDER_MODE				= 0x0C40

			PERSPECTIVE_CORRECTION_HINT		= 0x0C50
			POINT_SMOOTH_HINT			= 0x0C51
			LINE_SMOOTH_HINT			= 0x0C52
			POLYGON_SMOOTH_HINT			= 0x0C53
			FOG_HINT				= 0x0C54

			TEXTURE_GEN_S				= 0x0C60
			TEXTURE_GEN_T				= 0x0C61
			TEXTURE_GEN_R				= 0x0C62
			TEXTURE_GEN_Q				= 0x0C63

			PIXEL_MAP_I_TO_I			= 0x0C70
			PIXEL_MAP_S_TO_S			= 0x0C71
			PIXEL_MAP_I_TO_R			= 0x0C72
			PIXEL_MAP_I_TO_G			= 0x0C73
			PIXEL_MAP_I_TO_B			= 0x0C74
			PIXEL_MAP_I_TO_A			= 0x0C75
			PIXEL_MAP_R_TO_R			= 0x0C76
			PIXEL_MAP_G_TO_G			= 0x0C77
			PIXEL_MAP_B_TO_B			= 0x0C78
			PIXEL_MAP_A_TO_A			= 0x0C79

			PIXEL_MAP_I_TO_I_SIZE			= 0x0CB0
			PIXEL_MAP_S_TO_S_SIZE			= 0x0CB1
			PIXEL_MAP_I_TO_R_SIZE			= 0x0CB2
			PIXEL_MAP_I_TO_G_SIZE			= 0x0CB3
			PIXEL_MAP_I_TO_B_SIZE			= 0x0CB4
			PIXEL_MAP_I_TO_A_SIZE			= 0x0CB5
			PIXEL_MAP_R_TO_R_SIZE			= 0x0CB6
			PIXEL_MAP_G_TO_G_SIZE			= 0x0CB7
			PIXEL_MAP_B_TO_B_SIZE			= 0x0CB8
			PIXEL_MAP_A_TO_A_SIZE			= 0x0CB9

			UNPACK_SWAP_BYTES			= 0x0CF0
			UNPACK_LSB_FIRST			= 0x0CF1
			UNPACK_ROW_LENGTH			= 0x0CF2
			UNPACK_SKIP_ROWS			= 0x0CF3
			UNPACK_SKIP_PIXELS			= 0x0CF4
			UNPACK_ALIGNMENT			= 0x0CF5

			PACK_SWAP_BYTES				= 0x0D00
			PACK_LSB_FIRST				= 0x0D01
			PACK_ROW_LENGTH				= 0x0D02
			PACK_SKIP_ROWS				= 0x0D03
			PACK_SKIP_PIXELS			= 0x0D04
			PACK_ALIGNMENT				= 0x0D05

			MAP_COLOR				= 0x0D10
			MAP_STENCIL				= 0x0D11
			INDEX_SHIFT				= 0x0D12
			INDEX_OFFSET				= 0x0D13
			RED_SCALE				= 0x0D14
			RED_BIAS				= 0x0D15
			ZOOM_X					= 0x0D16
			ZOOM_Y					= 0x0D17
			GREEN_SCALE				= 0x0D18
			GREEN_BIAS				= 0x0D19
			BLUE_SCALE				= 0x0D1A
			BLUE_BIAS				= 0x0D1B
			ALPHA_SCALE				= 0x0D1C
			ALPHA_BIAS				= 0x0D1D
			DEPTH_SCALE				= 0x0D1E
			DEPTH_BIAS				= 0x0D1F

			MAX_EVAL_ORDER				= 0x0D30
			MAX_LIGHTS				= 0x0D31
			MAX_CLIP_PLANES				= 0x0D32
			MAX_TEXTURE_SIZE			= 0x0D33
			MAX_PIXEL_MAP_TABLE			= 0x0D34
			MAX_ATTRIB_STACK_DEPTH			= 0x0D35
			MAX_MODELVIEW_STACK_DEPTH		= 0x0D36
			MAX_NAME_STACK_DEPTH			= 0x0D37
			MAX_PROJECTION_STACK_DEPTH		= 0x0D38
			MAX_TEXTURE_STACK_DEPTH			= 0x0D39
			MAX_VIEWPORT_DIMS			= 0x0D3A
			MAX_CLIENT_ATTRIB_STACK_DEPTH		= 0x0D3B

			SUBPIXEL_BITS				= 0x0D50
			INDEX_BITS				= 0x0D51
			RED_BITS				= 0x0D52
			GREEN_BITS				= 0x0D53
			BLUE_BITS				= 0x0D54
			ALPHA_BITS				= 0x0D55
			DEPTH_BITS				= 0x0D56
			STENCIL_BITS				= 0x0D57
			ACCUM_RED_BITS				= 0x0D58
			ACCUM_GREEN_BITS			= 0x0D59
			ACCUM_BLUE_BITS				= 0x0D5A
			ACCUM_ALPHA_BITS			= 0x0D5B

			NAME_STACK_DEPTH			= 0x0D70

			AUTO_NORMAL				= 0x0D80

			MAP1_COLOR_4				= 0x0D90
			MAP1_INDEX				= 0x0D91
			MAP1_NORMAL				= 0x0D92
			MAP1_TEXTURE_COORD_1			= 0x0D93
			MAP1_TEXTURE_COORD_2			= 0x0D94
			MAP1_TEXTURE_COORD_3			= 0x0D95
			MAP1_TEXTURE_COORD_4			= 0x0D96
			MAP1_VERTEX_3				= 0x0D97
			MAP1_VERTEX_4				= 0x0D98

			MAP2_COLOR_4				= 0x0DB0
			MAP2_INDEX				= 0x0DB1
			MAP2_NORMAL				= 0x0DB2
			MAP2_TEXTURE_COORD_1			= 0x0DB3
			MAP2_TEXTURE_COORD_2			= 0x0DB4
			MAP2_TEXTURE_COORD_3			= 0x0DB5
			MAP2_TEXTURE_COORD_4			= 0x0DB6
			MAP2_VERTEX_3				= 0x0DB7
			MAP2_VERTEX_4				= 0x0DB8

			MAP1_GRID_DOMAIN			= 0x0DD0
			MAP1_GRID_SEGMENTS			= 0x0DD1
			MAP2_GRID_DOMAIN			= 0x0DD2
			MAP2_GRID_SEGMENTS			= 0x0DD3

			TEXTURE_1D				= 0x0DE0
			TEXTURE_2D				= 0x0DE1

			FEEDBACK_BUFFER_POINTER			= 0x0DF0
			FEEDBACK_BUFFER_SIZE			= 0x0DF1
			FEEDBACK_BUFFER_TYPE			= 0x0DF2

			SELECTION_BUFFER_POINTER		= 0x0DF3
			SELECTION_BUFFER_SIZE			= 0x0DF4

			use texture_object TEXTURE_BINDING_1D
			use texture_object TEXTURE_BINDING_2D
			use texture_object TEXTURE_BINDING_3D

			use vertex_array VERTEX_ARRAY
			use vertex_array NORMAL_ARRAY
			use vertex_array COLOR_ARRAY
			use vertex_array INDEX_ARRAY
			use vertex_array TEXTURE_COORD_ARRAY
			use vertex_array EDGE_FLAG_ARRAY
			use vertex_array VERTEX_ARRAY_SIZE
			use vertex_array VERTEX_ARRAY_TYPE
			use vertex_array VERTEX_ARRAY_STRIDE
			use vertex_array NORMAL_ARRAY_TYPE
			use vertex_array NORMAL_ARRAY_STRIDE
			use vertex_array COLOR_ARRAY_SIZE
			use vertex_array COLOR_ARRAY_TYPE
			use vertex_array COLOR_ARRAY_STRIDE
			use vertex_array INDEX_ARRAY_TYPE
			use vertex_array INDEX_ARRAY_STRIDE
			use vertex_array TEXTURE_COORD_ARRAY_SIZE
			use vertex_array TEXTURE_COORD_ARRAY_TYPE
			use vertex_array TEXTURE_COORD_ARRAY_STRIDE
			use vertex_array EDGE_FLAG_ARRAY_STRIDE

			use polygon_offset POLYGON_OFFSET_FACTOR
			use polygon_offset POLYGON_OFFSET_UNITS

			use color_table COLOR_TABLE
			use color_table POST_CONVOLUTION_COLOR_TABLE
			use color_table POST_COLOR_MATRIX_COLOR_TABLE

			use convolution CONVOLUTION_1D
			use convolution CONVOLUTION_2D
			use convolution SEPARABLE_2D
			use convolution POST_CONVOLUTION_RED_SCALE
			use convolution POST_CONVOLUTION_GREEN_SCALE
			use convolution POST_CONVOLUTION_BLUE_SCALE
			use convolution POST_CONVOLUTION_ALPHA_SCALE
			use convolution POST_CONVOLUTION_RED_BIAS
			use convolution POST_CONVOLUTION_GREEN_BIAS
			use convolution POST_CONVOLUTION_BLUE_BIAS
			use convolution POST_CONVOLUTION_ALPHA_BIAS

			use color_matrix COLOR_MATRIX
			use color_matrix COLOR_MATRIX_STACK_DEPTH
			use color_matrix MAX_COLOR_MATRIX_STACK_DEPTH
			use color_matrix POST_COLOR_MATRIX_RED_SCALE
			use color_matrix POST_COLOR_MATRIX_GREEN_SCALE
			use color_matrix POST_COLOR_MATRIX_BLUE_SCALE
			use color_matrix POST_COLOR_MATRIX_ALPHA_SCALE
			use color_matrix POST_COLOR_MATRIX_RED_BIAS
			use color_matrix POST_COLOR_MATRIX_GREEN_BIAS
			use color_matrix POST_COLOR_MATRIX_BLUE_BIAS
			use color_matrix POST_COLOR_MATRIX_ALPHA_BIAS

			use histogram HISTOGRAM
			use histogram MINMAX

			use draw_range_elements MAX_ELEMENTS_VERTICES
			use draw_range_elements MAX_ELEMENTS_INDICES

			use rescale_normal RESCALE_NORMAL

			use ARB_separate_specular_color LIGHT_MODEL_COLOR_CONTROL

			use texture3D PACK_SKIP_IMAGES
			use texture3D PACK_IMAGE_HEIGHT
			use texture3D UNPACK_SKIP_IMAGES
			use texture3D UNPACK_IMAGE_HEIGHT
			use texture3D TEXTURE_3D
			use texture3D MAX_3D_TEXTURE_SIZE

			use blend_color BLEND_COLOR

			use blend_minmax BLEND_EQUATION

			use multitexture ACTIVE_TEXTURE_ARB
			use multitexture CLIENT_ACTIVE_TEXTURE_ARB
			use multitexture MAX_TEXTURE_UNITS_ARB

GetTextureParameter	enum:
			use TextureParameter TEXTURE_MAG_FILTER
			use TextureParameter TEXTURE_MIN_FILTER
			use TextureParameter TEXTURE_WRAP_S
			use TextureParameter TEXTURE_WRAP_T
			TEXTURE_WIDTH				= 0x1000
			TEXTURE_HEIGHT				= 0x1001
			TEXTURE_INTERNAL_FORMAT			= 0x1003
			TEXTURE_BORDER_COLOR			= 0x1004
			TEXTURE_BORDER				= 0x1005

			use texture TEXTURE_RED_SIZE
			use texture TEXTURE_GREEN_SIZE
			use texture TEXTURE_BLUE_SIZE
			use texture TEXTURE_ALPHA_SIZE
			use texture TEXTURE_LUMINANCE_SIZE
			use texture TEXTURE_INTENSITY_SIZE

			use texture_object TEXTURE_PRIORITY
			use texture_object TEXTURE_RESIDENT

			use texture3D TEXTURE_DEPTH
			use texture3D TEXTURE_WRAP_R

			use texture_lod TEXTURE_MIN_LOD
			use texture_lod TEXTURE_MAX_LOD
			use texture_lod TEXTURE_BASE_LEVEL
			use texture_lod TEXTURE_MAX_LEVEL

HintMode		enum:
			DONT_CARE				= 0x1100
			FASTEST					= 0x1101
			NICEST					= 0x1102

HintTarget		enum:
			use GetTarget PERSPECTIVE_CORRECTION_HINT
			use GetTarget POINT_SMOOTH_HINT
			use GetTarget LINE_SMOOTH_HINT
			use GetTarget POLYGON_SMOOTH_HINT
			use GetTarget FOG_HINT

HistogramTarget		enum:
			use histogram HISTOGRAM
			use histogram PROXY_HISTOGRAM

IndexPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

LightModelColorControl	enum:
			use ARB_separate_specular_color SINGLE_COLOR
			use ARB_separate_specular_color SEPARATE_SPECULAR_COLOR

LightModelParameter	enum:
			use GetTarget LIGHT_MODEL_AMBIENT
			use GetTarget LIGHT_MODEL_LOCAL_VIEWER
			use GetTarget LIGHT_MODEL_TWO_SIDE
			use ARB_separate_specular_color LIGHT_MODEL_COLOR_CONTROL

# 0x4000-0x4FFF are reserved for light numbers
LightName		enum:
			LIGHT0					= 0x4000
			LIGHT1					= 0x4001
			LIGHT2					= 0x4002
			LIGHT3					= 0x4003
			LIGHT4					= 0x4004
			LIGHT5					= 0x4005
			LIGHT6					= 0x4006
			LIGHT7					= 0x4007

LightParameter		enum:
			AMBIENT					= 0x1200
			DIFFUSE					= 0x1201
			SPECULAR				= 0x1202
			POSITION				= 0x1203
			SPOT_DIRECTION				= 0x1204
			SPOT_EXPONENT				= 0x1205
			SPOT_CUTOFF				= 0x1206
			CONSTANT_ATTENUATION			= 0x1207
			LINEAR_ATTENUATION			= 0x1208
			QUADRATIC_ATTENUATION			= 0x1209

InterleavedArrays	enum:
			use vertex_array V2F
			use vertex_array V3F
			use vertex_array C4UB_V2F
			use vertex_array C4UB_V3F
			use vertex_array C3F_V3F
			use vertex_array N3F_V3F
			use vertex_array C4F_N3F_V3F
			use vertex_array T2F_V3F
			use vertex_array T4F_V4F
			use vertex_array T2F_C4UB_V3F
			use vertex_array T2F_C3F_V3F
			use vertex_array T2F_N3F_V3F
			use vertex_array T2F_C4F_N3F_V3F
			use vertex_array T4F_C4F_N3F_V4F

ListMode		enum:
			COMPILE					= 0x1300
			COMPILE_AND_EXECUTE			= 0x1301

ListNameType		enum:
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			use DataType 2_BYTES
			use DataType 3_BYTES
			use DataType 4_BYTES

# same names as X, GL style
LogicOp			enum:
			CLEAR					= 0x1500
			AND					= 0x1501
			AND_REVERSE				= 0x1502
			COPY					= 0x1503
			AND_INVERTED				= 0x1504
			NOOP					= 0x1505
			XOR					= 0x1506
			OR					= 0x1507
			NOR					= 0x1508
			EQUIV					= 0x1509
			INVERT					= 0x150A
			OR_REVERSE				= 0x150B
			COPY_INVERTED				= 0x150C
			OR_INVERTED				= 0x150D
			NAND					= 0x150E
			SET					= 0x150F

MapTarget		enum:
			use GetTarget MAP1_COLOR_4
			use GetTarget MAP1_INDEX
			use GetTarget MAP1_NORMAL
			use GetTarget MAP1_TEXTURE_COORD_1
			use GetTarget MAP1_TEXTURE_COORD_2
			use GetTarget MAP1_TEXTURE_COORD_3
			use GetTarget MAP1_TEXTURE_COORD_4
			use GetTarget MAP1_VERTEX_3
			use GetTarget MAP1_VERTEX_4
			use GetTarget MAP2_COLOR_4
			use GetTarget MAP2_INDEX
			use GetTarget MAP2_NORMAL
			use GetTarget MAP2_TEXTURE_COORD_1
			use GetTarget MAP2_TEXTURE_COORD_2
			use GetTarget MAP2_TEXTURE_COORD_3
			use GetTarget MAP2_TEXTURE_COORD_4
			use GetTarget MAP2_VERTEX_3
			use GetTarget MAP2_VERTEX_4

MaterialFace		enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

MaterialParameter	enum:
			EMISSION				= 0x1600
			SHININESS				= 0x1601
			AMBIENT_AND_DIFFUSE			= 0x1602
			COLOR_INDEXES				= 0x1603
			use LightProperty AMBIENT
			use LightProperty DIFFUSE
			use LightProperty SPECULAR

MatrixMode		enum:
			MODELVIEW				= 0x1700
			PROJECTION				= 0x1701
			TEXTURE					= 0x1702

MeshMode1		enum:
			use PolygonMode POINT
			use PolygonMode LINE

MeshMode2		enum:
			use PolygonMode POINT
			use PolygonMode LINE
			use PolygonMode FILL

MinmaxTarget		enum:
			use histogram MINMAX

NormalPointerType	enum:
			use DataType BYTE
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

PixelCopyType		enum:
			COLOR					= 0x1800
			DEPTH					= 0x1801
			STENCIL					= 0x1802

PixelFormat		enum:
			COLOR_INDEX				= 0x1900
			STENCIL_INDEX				= 0x1901
			DEPTH_COMPONENT				= 0x1902
			RED					= 0x1903
			GREEN					= 0x1904
			BLUE					= 0x1905
			ALPHA					= 0x1906
			RGB					= 0x1907
			RGBA					= 0x1908
			LUMINANCE				= 0x1909
			LUMINANCE_ALPHA				= 0x190A
			use abgr ABGR

PixelInternalFormat	enum:
			use texture ALPHA4
			use texture ALPHA8
			use texture ALPHA12
			use texture ALPHA16
			use texture LUMINANCE4
			use texture LUMINANCE8
			use texture LUMINANCE12
			use texture LUMINANCE16
			use texture LUMINANCE4_ALPHA4
			use texture LUMINANCE6_ALPHA2
			use texture LUMINANCE8_ALPHA8
			use texture LUMINANCE12_ALPHA4
			use texture LUMINANCE12_ALPHA12
			use texture LUMINANCE16_ALPHA16
			use texture INTENSITY
			use texture INTENSITY4
			use texture INTENSITY8
			use texture INTENSITY12
			use texture INTENSITY16
			use texture R3_G3_B2
			use texture RGB4
			use texture RGB5
			use texture RGB8
			use texture RGB10
			use texture RGB12
			use texture RGB16
			use texture RGBA2
			use texture RGBA4
			use texture RGB5_A1
			use texture RGBA8
			use texture RGB10_A2
			use texture RGBA12
			use texture RGBA16

PixelMap		enum:
			use GetTarget PIXEL_MAP_I_TO_I
			use GetTarget PIXEL_MAP_S_TO_S
			use GetTarget PIXEL_MAP_I_TO_R
			use GetTarget PIXEL_MAP_I_TO_G
			use GetTarget PIXEL_MAP_I_TO_B
			use GetTarget PIXEL_MAP_I_TO_A
			use GetTarget PIXEL_MAP_R_TO_R
			use GetTarget PIXEL_MAP_G_TO_G
			use GetTarget PIXEL_MAP_B_TO_B
			use GetTarget PIXEL_MAP_A_TO_A

PixelStore		enum:
			use GetTarget UNPACK_SWAP_BYTES
			use GetTarget UNPACK_LSB_FIRST
			use GetTarget UNPACK_ROW_LENGTH
			use GetTarget UNPACK_SKIP_ROWS
			use GetTarget UNPACK_SKIP_PIXELS
			use GetTarget UNPACK_ALIGNMENT
			use GetTarget PACK_SWAP_BYTES
			use GetTarget PACK_LSB_FIRST
			use GetTarget PACK_ROW_LENGTH
			use GetTarget PACK_SKIP_ROWS
			use GetTarget PACK_SKIP_PIXELS
			use GetTarget PACK_ALIGNMENT

			use texture3D PACK_SKIP_IMAGES
			use texture3D PACK_IMAGE_HEIGHT
			use texture3D UNPACK_SKIP_IMAGES
			use texture3D UNPACK_IMAGE_HEIGHT

PixelTransfer		enum:
			use GetTarget MAP_COLOR
			use GetTarget MAP_STENCIL
			use GetTarget INDEX_SHIFT
			use GetTarget INDEX_OFFSET
			use GetTarget RED_SCALE
			use GetTarget RED_BIAS
			use GetTarget GREEN_SCALE
			use GetTarget GREEN_BIAS
			use GetTarget BLUE_SCALE
			use GetTarget BLUE_BIAS
			use GetTarget ALPHA_SCALE
			use GetTarget ALPHA_BIAS
			use GetTarget DEPTH_SCALE
			use GetTarget DEPTH_BIAS

			use convolution POST_CONVOLUTION_RED_SCALE
			use convolution POST_CONVOLUTION_GREEN_SCALE
			use convolution POST_CONVOLUTION_BLUE_SCALE
			use convolution POST_CONVOLUTION_ALPHA_SCALE
			use convolution POST_CONVOLUTION_RED_BIAS
			use convolution POST_CONVOLUTION_GREEN_BIAS
			use convolution POST_CONVOLUTION_BLUE_BIAS
			use convolution POST_CONVOLUTION_ALPHA_BIAS

			use color_matrix POST_COLOR_MATRIX_RED_SCALE
			use color_matrix POST_COLOR_MATRIX_GREEN_SCALE
			use color_matrix POST_COLOR_MATRIX_BLUE_SCALE
			use color_matrix POST_COLOR_MATRIX_ALPHA_SCALE
			use color_matrix POST_COLOR_MATRIX_RED_BIAS
			use color_matrix POST_COLOR_MATRIX_GREEN_BIAS
			use color_matrix POST_COLOR_MATRIX_BLUE_BIAS
			use color_matrix POST_COLOR_MATRIX_ALPHA_BIAS

PixelType		enum:
			BITMAP					= 0x1A00
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			use bgra BGR
			use bgra BGRA
			use packed_pixels UNSIGNED_BYTE_3_3_2
			use packed_pixels UNSIGNED_SHORT_4_4_4_4
			use packed_pixels UNSIGNED_SHORT_5_5_5_1
			use packed_pixels UNSIGNED_INT_8_8_8_8
			use packed_pixels UNSIGNED_INT_10_10_10_2
			use packed_pixels UNSIGNED_SHORT_5_6_5
			use packed_pixels UNSIGNED_BYTE_2_3_3_REV
			use packed_pixels UNSIGNED_SHORT_5_6_5_REV
			use packed_pixels UNSIGNED_SHORT_4_4_4_4_REV
			use packed_pixels UNSIGNED_SHORT_1_5_5_5_REV
			use packed_pixels UNSIGNED_INT_8_8_8_8_REV
			use packed_pixels UNSIGNED_INT_2_10_10_10_REV

PolygonMode		enum:
			POINT					= 0x1B00
			LINE					= 0x1B01
			FILL					= 0x1B02

ReadBufferMode		enum:
			use DrawBufferMode FRONT_LEFT
			use DrawBufferMode FRONT_RIGHT
			use DrawBufferMode BACK_LEFT
			use DrawBufferMode BACK_RIGHT
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode LEFT
			use DrawBufferMode RIGHT
			use DrawBufferMode AUX0
			use DrawBufferMode AUX1
			use DrawBufferMode AUX2
			use DrawBufferMode AUX3

RenderingMode		enum:
			RENDER					= 0x1C00
			FEEDBACK				= 0x1C01
			SELECT					= 0x1C02

SeparableTarget		enum:
			use convolution SEPARABLE_2D

ShadingModel		enum:
			FLAT					= 0x1D00
			SMOOTH					= 0x1D01

StencilFunction		enum:
			use AlphaFunction NEVER
			use AlphaFunction LESS
			use AlphaFunction EQUAL
			use AlphaFunction LEQUAL
			use AlphaFunction GREATER
			use AlphaFunction NOTEQUAL
			use AlphaFunction GEQUAL
			use AlphaFunction ALWAYS

StencilOp		enum:
			use BlendingFactorDest ZERO
			KEEP					= 0x1E00
			REPLACE					= 0x1E01
			INCR					= 0x1E02
			DECR					= 0x1E03
			use LogicOp INVERT

StringName		enum:
			VENDOR					= 0x1F00
			RENDERER				= 0x1F01
			VERSION					= 0x1F02
			EXTENSIONS				= 0x1F03

TextureCoordName	enum:
			S					= 0x2000
			T					= 0x2001
			R					= 0x2002
			Q					= 0x2003

TexCoordPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

TextureEnvMode		enum:
			MODULATE				= 0x2100
			DECAL					= 0x2101
			use GetTarget BLEND
			use StencilOp REPLACE

TextureEnvParameter	enum:
			TEXTURE_ENV_MODE			= 0x2200
			TEXTURE_ENV_COLOR			= 0x2201

TextureEnvTarget	enum:
			TEXTURE_ENV				= 0x2300

TextureGenMode		enum:
			EYE_LINEAR				= 0x2400
			OBJECT_LINEAR				= 0x2401
			SPHERE_MAP				= 0x2402

TextureGenParameter	enum:
			TEXTURE_GEN_MODE			= 0x2500
			OBJECT_PLANE				= 0x2501
			EYE_PLANE				= 0x2502

TextureMagFilter	enum:
			NEAREST					= 0x2600
			LINEAR					= 0x2601

TextureMinFilter	enum:
			use TextureMagFilter NEAREST
			use TextureMagFilter LINEAR
			NEAREST_MIPMAP_NEAREST			= 0x2700
			LINEAR_MIPMAP_NEAREST			= 0x2701
			NEAREST_MIPMAP_LINEAR			= 0x2702
			LINEAR_MIPMAP_LINEAR			= 0x2703

TextureParameterName	enum:
			TEXTURE_MAG_FILTER			= 0x2800
			TEXTURE_MIN_FILTER			= 0x2801
			TEXTURE_WRAP_S				= 0x2802
			TEXTURE_WRAP_T				= 0x2803
			use GetTextureParameter TEXTURE_BORDER_COLOR
			use texture_object TEXTURE_PRIORITY
			use texture3D TEXTURE_WRAP_R
			use texture_lod TEXTURE_MIN_LOD
			use texture_lod TEXTURE_MAX_LOD
			use texture_lod TEXTURE_BASE_LEVEL
			use texture_lod TEXTURE_MAX_LEVEL

TextureTarget		enum:
			use GetTarget TEXTURE_1D
			use GetTarget TEXTURE_2D
			use texture PROXY_TEXTURE_1D
			use texture PROXY_TEXTURE_2D
			use texture3D TEXTURE_3D
			use texture3D PROXY_TEXTURE_3D

TextureUnit		enum:
			use multitexture TEXTURE0_ARB
			use multitexture TEXTURE1_ARB
			use multitexture TEXTURE2_ARB
			use multitexture TEXTURE3_ARB
			use multitexture TEXTURE4_ARB
			use multitexture TEXTURE5_ARB
			use multitexture TEXTURE6_ARB
			use multitexture TEXTURE7_ARB
			use multitexture TEXTURE8_ARB
			use multitexture TEXTURE9_ARB
			use multitexture TEXTURE10_ARB
			use multitexture TEXTURE11_ARB
			use multitexture TEXTURE12_ARB
			use multitexture TEXTURE13_ARB
			use multitexture TEXTURE14_ARB
			use multitexture TEXTURE15_ARB
			use multitexture TEXTURE16_ARB
			use multitexture TEXTURE17_ARB
			use multitexture TEXTURE18_ARB
			use multitexture TEXTURE19_ARB
			use multitexture TEXTURE20_ARB
			use multitexture TEXTURE21_ARB
			use multitexture TEXTURE22_ARB
			use multitexture TEXTURE23_ARB
			use multitexture TEXTURE24_ARB
			use multitexture TEXTURE25_ARB
			use multitexture TEXTURE26_ARB
			use multitexture TEXTURE27_ARB
			use multitexture TEXTURE28_ARB
			use multitexture TEXTURE29_ARB
			use multitexture TEXTURE30_ARB
			use multitexture TEXTURE31_ARB

TextureWrapMode		enum:
			CLAMP					= 0x2900
			REPEAT					= 0x2901
			use texture_edge_clamp CLAMP_TO_EDGE

VertexPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

######################################################################
# 0x2A30-0x7FFF are reserved for future expansion
######################################################################

######################################################################
# 1.1
######################################################################

ClientAttribMask	enum:
			CLIENT_PIXEL_STORE_BIT			= 0x00000001
			CLIENT_VERTEX_ARRAY_BIT			= 0x00000002
			CLIENT_ALL_ATTRIB_BITS			= 0xffffffff

polygon_offset		enum:
			POLYGON_OFFSET_FACTOR			= 0x8038
			POLYGON_OFFSET_UNITS			= 0x2A00
			POLYGON_OFFSET_POINT			= 0x2A01
			POLYGON_OFFSET_LINE			= 0x2A02
			POLYGON_OFFSET_FILL			= 0x8037

texture			enum:
			ALPHA4					= 0x803B
			ALPHA8					= 0x803C
			ALPHA12					= 0x803D
			ALPHA16					= 0x803E
			LUMINANCE4				= 0x803F
			LUMINANCE8				= 0x8040
			LUMINANCE12				= 0x8041
			LUMINANCE16				= 0x8042
			LUMINANCE4_ALPHA4			= 0x8043
			LUMINANCE6_ALPHA2			= 0x8044
			LUMINANCE8_ALPHA8			= 0x8045
			LUMINANCE12_ALPHA4			= 0x8046
			LUMINANCE12_ALPHA12			= 0x8047
			LUMINANCE16_ALPHA16			= 0x8048
			INTENSITY				= 0x8049
			INTENSITY4				= 0x804A
			INTENSITY8				= 0x804B
			INTENSITY12				= 0x804C
			INTENSITY16				= 0x804D
			R3_G3_B2				= 0x2A10
			RGB4					= 0x804F
			RGB5					= 0x8050
			RGB8					= 0x8051
			RGB10					= 0x8052
			RGB12					= 0x8053
			RGB16					= 0x8054
			RGBA2					= 0x8055
			RGBA4					= 0x8056
			RGB5_A1					= 0x8057
			RGBA8					= 0x8058
			RGB10_A2				= 0x8059
			RGBA12					= 0x805A
			RGBA16					= 0x805B
			TEXTURE_RED_SIZE			= 0x805C
			TEXTURE_GREEN_SIZE			= 0x805D
			TEXTURE_BLUE_SIZE			= 0x805E
			TEXTURE_ALPHA_SIZE			= 0x805F
			TEXTURE_LUMINANCE_SIZE			= 0x8060
			TEXTURE_INTENSITY_SIZE			= 0x8061
			PROXY_TEXTURE_1D			= 0x8063
			PROXY_TEXTURE_2D			= 0x8064

texture_object		enum:
			TEXTURE_PRIORITY			= 0x8066
			TEXTURE_RESIDENT			= 0x8067
			TEXTURE_BINDING_1D			= 0x8068
			TEXTURE_BINDING_2D			= 0x8069
			TEXTURE_BINDING_3D			= 0x806A

vertex_array		enum:
			VERTEX_ARRAY				= 0x8074
			NORMAL_ARRAY				= 0x8075
			COLOR_ARRAY				= 0x8076
			INDEX_ARRAY				= 0x8077
			TEXTURE_COORD_ARRAY			= 0x8078
			EDGE_FLAG_ARRAY				= 0x8079
			VERTEX_ARRAY_SIZE			= 0x807A
			VERTEX_ARRAY_TYPE			= 0x807B
			VERTEX_ARRAY_STRIDE			= 0x807C
			NORMAL_ARRAY_TYPE			= 0x807E
			NORMAL_ARRAY_STRIDE			= 0x807F
			COLOR_ARRAY_SIZE			= 0x8081
			COLOR_ARRAY_TYPE			= 0x8082
			COLOR_ARRAY_STRIDE			= 0x8083
			INDEX_ARRAY_TYPE			= 0x8085
			INDEX_ARRAY_STRIDE			= 0x8086
			TEXTURE_COORD_ARRAY_SIZE		= 0x8088
			TEXTURE_COORD_ARRAY_TYPE		= 0x8089
			TEXTURE_COORD_ARRAY_STRIDE		= 0x808A
			EDGE_FLAG_ARRAY_STRIDE			= 0x808C
			VERTEX_ARRAY_POINTER			= 0x808E
			NORMAL_ARRAY_POINTER			= 0x808F
			COLOR_ARRAY_POINTER			= 0x8090
			INDEX_ARRAY_POINTER			= 0x8091
			TEXTURE_COORD_ARRAY_POINTER		= 0x8092
			EDGE_FLAG_ARRAY_POINTER			= 0x8093
			V2F					= 0x2A20
			V3F					= 0x2A21
			C4UB_V2F				= 0x2A22
			C4UB_V3F				= 0x2A23
			C3F_V3F					= 0x2A24
			N3F_V3F					= 0x2A25
			C4F_N3F_V3F				= 0x2A26
			T2F_V3F					= 0x2A27
			T4F_V4F					= 0x2A28
			T2F_C4UB_V3F				= 0x2A29
			T2F_C3F_V3F				= 0x2A2A
			T2F_N3F_V3F				= 0x2A2B
			T2F_C4F_N3F_V3F				= 0x2A2C
			T4F_C4F_N3F_V4F				= 0x2A2D

######################################################################
# 1.2
######################################################################

bgra			enum:
			BGR					= 0x80E0
			BGRA					= 0x80E1

blend_color		enum:
			CONSTANT_COLOR				= 0x8001
			ONE_MINUS_CONSTANT_COLOR		= 0x8002
			CONSTANT_ALPHA				= 0x8003
			ONE_MINUS_CONSTANT_ALPHA		= 0x8004
			BLEND_COLOR				= 0x8005 # 4 F

blend_minmax		enum:
			FUNC_ADD				= 0x8006
			MIN					= 0x8007
			MAX					= 0x8008
			BLEND_EQUATION				= 0x8009 # 1 I

blend_subtract		enum:
			FUNC_SUBTRACT				= 0x800A
			FUNC_REVERSE_SUBTRACT			= 0x800B

color_matrix		enum:
			COLOR_MATRIX				= 0x80B1 # 16 F
			COLOR_MATRIX_STACK_DEPTH		= 0x80B2 # 1 I
			MAX_COLOR_MATRIX_STACK_DEPTH		= 0x80B3 # 1 I
			POST_COLOR_MATRIX_RED_SCALE		= 0x80B4 # 1 F
			POST_COLOR_MATRIX_GREEN_SCALE		= 0x80B5 # 1 F
			POST_COLOR_MATRIX_BLUE_SCALE		= 0x80B6 # 1 F
			POST_COLOR_MATRIX_ALPHA_SCALE		= 0x80B7 # 1 F
			POST_COLOR_MATRIX_RED_BIAS		= 0x80B8 # 1 F
			POST_COLOR_MATRIX_GREEN_BIAS		= 0x80B9 # 1 F
			POST_COLOR_MATRIX_BLUE_BIAS		= 0x80BA # 1 F
			POST_COLOR_MATRIX_ALPHA_BIAS		= 0x80BB # 1 F

color_table		enum:
			COLOR_TABLE				= 0x80D0 # 1 I
			POST_CONVOLUTION_COLOR_TABLE		= 0x80D1 # 1 I
			POST_COLOR_MATRIX_COLOR_TABLE		= 0x80D2 # 1 I
			PROXY_COLOR_TABLE			= 0x80D3
			PROXY_POST_CONVOLUTION_COLOR_TABLE	= 0x80D4
			PROXY_POST_COLOR_MATRIX_COLOR_TABLE	= 0x80D5
			COLOR_TABLE_SCALE			= 0x80D6
			COLOR_TABLE_BIAS			= 0x80D7
			COLOR_TABLE_FORMAT			= 0x80D8
			COLOR_TABLE_WIDTH			= 0x80D9
			COLOR_TABLE_RED_SIZE			= 0x80DA
			COLOR_TABLE_GREEN_SIZE			= 0x80DB
			COLOR_TABLE_BLUE_SIZE			= 0x80DC
			COLOR_TABLE_ALPHA_SIZE			= 0x80DD
			COLOR_TABLE_LUMINANCE_SIZE		= 0x80DE
			COLOR_TABLE_INTENSITY_SIZE		= 0x80DF

convolution		enum:
			CONVOLUTION_1D				= 0x8010 # 1 I
			CONVOLUTION_2D				= 0x8011 # 1 I
			SEPARABLE_2D				= 0x8012 # 1 I
			CONVOLUTION_BORDER_MODE			= 0x8013
			CONVOLUTION_FILTER_SCALE		= 0x8014
			CONVOLUTION_FILTER_BIAS			= 0x8015
			REDUCE					= 0x8016
			CONVOLUTION_FORMAT			= 0x8017
			CONVOLUTION_WIDTH			= 0x8018
			CONVOLUTION_HEIGHT			= 0x8019
			MAX_CONVOLUTION_WIDTH			= 0x801A
			MAX_CONVOLUTION_HEIGHT			= 0x801B
			POST_CONVOLUTION_RED_SCALE		= 0x801C # 1 F
			POST_CONVOLUTION_GREEN_SCALE		= 0x801D # 1 F
			POST_CONVOLUTION_BLUE_SCALE		= 0x801E # 1 F
			POST_CONVOLUTION_ALPHA_SCALE		= 0x801F # 1 F
			POST_CONVOLUTION_RED_BIAS		= 0x8020 # 1 F
			POST_CONVOLUTION_GREEN_BIAS		= 0x8021 # 1 F
			POST_CONVOLUTION_BLUE_BIAS		= 0x8022 # 1 F
			POST_CONVOLUTION_ALPHA_BIAS		= 0x8023 # 1 F
			CONSTANT_BORDER				= 0x8151
			REPLICATE_BORDER			= 0x8153
			CONVOLUTION_BORDER_COLOR		= 0x8154

draw_range_elements	enum:
			MAX_ELEMENTS_VERTICES			= 0x80E8
			MAX_ELEMENTS_INDICES			= 0x80E9

histogram		enum:
			HISTOGRAM				= 0x8024 # 1 I
			PROXY_HISTOGRAM				= 0x8025
			HISTOGRAM_WIDTH				= 0x8026
			HISTOGRAM_FORMAT			= 0x8027
			HISTOGRAM_RED_SIZE			= 0x8028
			HISTOGRAM_GREEN_SIZE			= 0x8029
			HISTOGRAM_BLUE_SIZE			= 0x802A
			HISTOGRAM_ALPHA_SIZE			= 0x802B
			HISTOGRAM_LUMINANCE_SIZE		= 0x802C
			HISTOGRAM_SINK				= 0x802D
			MINMAX					= 0x802E # 1 I
			MINMAX_FORMAT				= 0x802F
			MINMAX_SINK				= 0x8030
			TABLE_TOO_LARGE				= 0x8031

packed_pixels		enum:
			UNSIGNED_BYTE_3_3_2			= 0x8032
			UNSIGNED_SHORT_4_4_4_4			= 0x8033
			UNSIGNED_SHORT_5_5_5_1			= 0x8034
			UNSIGNED_INT_8_8_8_8			= 0x8035
			UNSIGNED_INT_10_10_10_2			= 0x8036
			UNSIGNED_BYTE_2_3_3_REV			= 0x8362
			UNSIGNED_SHORT_5_6_5			= 0x8363
			UNSIGNED_SHORT_5_6_5_REV		= 0x8364
			UNSIGNED_SHORT_4_4_4_4_REV		= 0x8365
			UNSIGNED_SHORT_1_5_5_5_REV		= 0x8366
			UNSIGNED_INT_8_8_8_8_REV		= 0x8367
			UNSIGNED_INT_2_10_10_10_REV		= 0x8368

rescale_normal		enum:
			RESCALE_NORMAL				= 0x803A # 1 I

separate_specular_color enum:
			LIGHT_MODEL_COLOR_CONTROL		= 0x81F8 # 1 I
			SINGLE_COLOR				= 0x81F9
			SEPARATE_SPECULAR_COLOR			= 0x81FA

texture3D		enum:
			PACK_SKIP_IMAGES			= 0x806B # 1 I
			PACK_IMAGE_HEIGHT			= 0x806C # 1 F
			UNPACK_SKIP_IMAGES			= 0x806D # 1 I
			UNPACK_IMAGE_HEIGHT			= 0x806E # 1 F
			TEXTURE_3D				= 0x806F # 1 I
			PROXY_TEXTURE_3D			= 0x8070
			TEXTURE_DEPTH				= 0x8071
			TEXTURE_WRAP_R				= 0x8072
			MAX_3D_TEXTURE_SIZE			= 0x8073 # 1 I

texture_edge_clamp	enum:
			CLAMP_TO_EDGE				= 0x812F

texture_lod		enum:
			TEXTURE_MIN_LOD				= 0x813A
			TEXTURE_MAX_LOD				= 0x813B
			TEXTURE_BASE_LEVEL			= 0x813C
			TEXTURE_MAX_LEVEL			= 0x813D

GetTarget1_2		enum:
			SMOOTH_POINT_SIZE_RANGE			= 0x0B12
			SMOOTH_POINT_SIZE_GRANULARITY		= 0x0B13
			SMOOTH_LINE_WIDTH_RANGE			= 0x0B22
			SMOOTH_LINE_WIDTH_GRANULARITY		= 0x0B23
			ALIASED_POINT_SIZE_RANGE		= 0x846D # 2 F
			ALIASED_LINE_WIDTH_RANGE		= 0x846E # 2 F

######################################################################
# 1.2.1 (ARB_multitexture)
######################################################################

multitexture		enum:
			TEXTURE0_ARB				= 0x84C0
			TEXTURE1_ARB				= 0x84C1
			TEXTURE2_ARB				= 0x84C2
			TEXTURE3_ARB				= 0x84C3
			TEXTURE4_ARB				= 0x84C4
			TEXTURE5_ARB				= 0x84C5
			TEXTURE6_ARB				= 0x84C6
			TEXTURE7_ARB				= 0x84C7
			TEXTURE8_ARB				= 0x84C8
			TEXTURE9_ARB				= 0x84C9
			TEXTURE10_ARB				= 0x84CA
			TEXTURE11_ARB				= 0x84CB
			TEXTURE12_ARB				= 0x84CC
			TEXTURE13_ARB				= 0x84CD
			TEXTURE14_ARB				= 0x84CE
			TEXTURE15_ARB				= 0x84CF
			TEXTURE16_ARB				= 0x84D0
			TEXTURE17_ARB				= 0x84D1
			TEXTURE18_ARB				= 0x84D2
			TEXTURE19_ARB				= 0x84D3
			TEXTURE20_ARB				= 0x84D4
			TEXTURE21_ARB				= 0x84D5
			TEXTURE22_ARB				= 0x84D6
			TEXTURE23_ARB				= 0x84D7
			TEXTURE24_ARB				= 0x84D8
			TEXTURE25_ARB				= 0x84D9
			TEXTURE26_ARB				= 0x84DA
			TEXTURE27_ARB				= 0x84DB
			TEXTURE28_ARB				= 0x84DC
			TEXTURE29_ARB				= 0x84DD
			TEXTURE30_ARB				= 0x84DE
			TEXTURE31_ARB				= 0x84DF
			ACTIVE_TEXTURE_ARB			= 0x84E0
			CLIENT_ACTIVE_TEXTURE_ARB		= 0x84E1
			MAX_TEXTURE_UNITS_ARB			= 0x84E2


######################################################################
# Extensions
######################################################################

Extensions		define:
			EXT_abgr				= 1
			EXT_blend_color				= 1
			EXT_blend_minmax			= 1
			EXT_blend_subtract			= 1
                        EXT_texture_env_combine                 = 1
                        EXT_texture_env_add                     = 1

EXT_abgr		enum:
			ABGR_EXT				= 0x8000

EXT_blend_color		enum:
			CONSTANT_COLOR_EXT			= 0x8001
			ONE_MINUS_CONSTANT_COLOR_EXT		= 0x8002
			CONSTANT_ALPHA_EXT			= 0x8003
			ONE_MINUS_CONSTANT_ALPHA_EXT		= 0x8004
			BLEND_COLOR_EXT				= 0x8005

EXT_blend_minmax	enum:
			FUNC_ADD_EXT				= 0x8006
			MIN_EXT					= 0x8007
			MAX_EXT					= 0x8008
			BLEND_EQUATION_EXT			= 0x8009

EXT_blend_subtract	enum:
			FUNC_SUBTRACT_EXT			= 0x800A
			FUNC_REVERSE_SUBTRACT_EXT		= 0x800B

EXT_texture_env_combine enum:
                        COMBINE_EXT                             = 0x8570
                        COMBINE_RGB_EXT                         = 0x8571
                        COMBINE_ALPHA_EXT                       = 0x8572
                        RGB_SCALE_EXT                           = 0x8573
                        ADD_SIGNED_EXT                          = 0x8574
                        INTERPOLATE_EXT                         = 0x8575
                        CONSTANT_EXT                            = 0x8576
                        PRIMARY_COLOR_EXT                       = 0x8577
                        PREVIOUS_EXT                            = 0x8578
                        SOURCE0_RGB_EXT                         = 0x8580
                        SOURCE1_RGB_EXT                         = 0x8581
                        SOURCE2_RGB_EXT                         = 0x8582
                        SOURCE0_ALPHA_EXT                       = 0x8588
                        SOURCE1_ALPHA_EXT                       = 0x8589
                        SOURCE2_ALPHA_EXT                       = 0x858A
                        OPERAND0_RGB_EXT                        = 0x8590
                        OPERAND1_RGB_EXT                        = 0x8591
                        OPERAND2_RGB_EXT                        = 0x8592
                        OPERAND0_ALPHA_EXT                      = 0x8598
                        OPERAND1_ALPHA_EXT                      = 0x8599
                        OPERAND2_ALPHA_EXT                      = 0x859A
