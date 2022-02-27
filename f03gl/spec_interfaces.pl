#!/usr/bin/perl -w

#  Construct a file of Fortran interfaces for the GL or GLU parameters and
#  procedures, using the GL.spec and GLU.spec files as the starting-point.

#  Changes 2009-10-29:
#  PRIVATE is the default. Help text improved.


$path=".";
$prefix="gl";
$default=0;
$glu=0;
$bozinit=0;
$PRIVATE=1;
$module="f03gl";
while ( @ARGV ) {
  $flag=shift;
  $glu=0, next if $flag eq "-gl";
  $glu=1, next if $flag eq "-glu";
  $default=1, next if $flag eq "-d";
  $path=shift, next if $flag eq "-p";
  $PRIVATE=1, next if $flag eq "--PRIVATE";
  $PRIVATE=0, next if $flag eq "--PUBLIC";
  $bozinit=1, next if $flag eq "--bozinit";
  $module=shift, next if $flag eq "-m" or $flag eq "--module";
  &help, exit; # for anything else
}

if ($PRIVATE) {
   $ACCESSIBILITY="PRIVATE";
   $PARAMETER="PARAMETER, PUBLIC";
}
else
 {
   $ACCESSIBILITY="PUBLIC";
   $PARAMETER="PARAMETER";
}

if ($glu) {
  $prefix="glu";
  $library="GLU";
  open (STDOUT,">${module}_glu.f90");
}
else {
  $library="GL";
  open (STDOUT,">${module}_gl.f90");
  print "MODULE ${module}_kinds
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
PUBLIC

! Kind parameters
! Integer types:
INTEGER, PARAMETER :: GLbyte=C_SIGNED_CHAR, GLshort=C_SHORT,            &
    GLint=C_INT, GLsizei=C_INT, GLboolean=C_SIGNED_CHAR, GLenum=C_INT,  &
    GLbitfield=C_INT, GLcint=C_INT, GLubyte=C_SIGNED_CHAR, &
    GLushort=C_SHORT, GLuint=C_INT
! Real types:
INTEGER, PARAMETER :: GLdouble=C_DOUBLE, GLfloat=C_FLOAT, GLclampf=C_FLOAT, &
    GLclampd=C_DOUBLE

END MODULE ${module}_kinds
";
}

print "MODULE ${module}_${library}
USE, INTRINSIC :: ISO_C_BINDING
USE ${module}_kinds
IMPLICIT NONE
$ACCESSIBILITY
";
if ( !$glu ) {
print "

!  Version values
INTEGER(GLenum), $PARAMETER :: GL_VERSION_1_1=1
INTEGER(GLenum), $PARAMETER :: GL_VERSION_1_2=1

";
}

#  If the -d flag is set, anything not in this table is assumed to be
#  INTEGER(GLenum).
#  (There shouldn't be any such cases after the enum.spec file has
#   been scanned, but in fact it seems that there are a few.)
%map = (
AttribMask => "INTEGER(GLbitfield)",
BeginMode => "INTEGER(GLenum)",
BlendingFactorDest => "INTEGER(GLenum)",
BlendingFactorSrc => "INTEGER(GLenum)",
Boolean   => "INTEGER(GLboolean)",
CheckedFloat32 => "REAL(GLfloat)",
CheckedInt32 => "INTEGER(GLint)",
ClampedColorF => "REAL(GLclampf)",
ClampedFloat32 => "REAL(GLclampf)",
ClampedFloat64 => "REAL(GLclampd)",
ClampedStencilValue => "INTEGER(GLint)",
ClearBufferMask => "INTEGER(GLbitfield)",
ClientAttribMask => "INTEGER(GLbitfield)",
ClipPlaneName => "INTEGER(GLenum)",
ColorD    => "REAL(GLdouble)",
ColorF    => "REAL(GLfloat)",
ColorB    => "INTEGER(GLbyte)",
ColorI    => "INTEGER(GLint)",
ColorS    => "INTEGER(GLshort)",
ColorUB   => "INTEGER(GLbyte)",
ColorUI   => "INTEGER(GLint)",
ColorUS   => "INTEGER(GLshort)",
ColorIndexValueD => "REAL(GLdouble)",
ColorIndexValueF => "REAL(GLfloat)",
ColorIndexValueI => "INTEGER(GLint)",
ColorIndexValueS => "INTEGER(GLshort)",
ColorIndexValueUB => "INTEGER(GLubyte)",
ColorMaterialParameter => "INTEGER(GLenum)",
CoordD    => "REAL(GLdouble)",
CoordF    => "REAL(GLfloat)",
CoordI    => "INTEGER(GLint)",
CoordS    => "INTEGER(GLshort)",
CullFaceMode => "INTEGER(GLenum)",
EnableCap => "INTEGER(GLenum)",
ErrorCode => "INTEGER(GLenum)",
FeedbackElement => "REAL(GLfloat)",
FeedBackToken => "REAL(GLfloat)",
Float32   => "REAL(GLfloat)",
Float32Pointer   => "REAL(GLfloat), DIMENSION(*)",
Float64   => "REAL(GLdouble)",
Float64Pointer   => "REAL(GLdouble), DIMENSION(*)",
FrontFaceDirection => "INTEGER(GLenum)",
FunctionPointer => "TYPE(C_FUNPTR)",
Int8      => "INTEGER(GLbyte)",
Int16     => "INTEGER(GLshort)",
Int32     => "INTEGER(GLint)",
LineStipple => "INTEGER(GLushort)",
List      => "INTEGER(GLint)",
ListMode  => "INTEGER(GLenum)",
ListNameType => "INTEGER(GLenum)",
MaskedColorIndexValueF => "REAL(GLfloat)",
MaskedColorIndexValueI => "INTEGER(GLuint)",
MaskedStencilValue => "INTEGER(GLuint)",
MaterialFace => "INTEGER(GLenum)",
NurbsObj => "TYPE(C_PTR)",
PixelInternalFormat => "INTEGER(GLint)",
QuadricDrawStyle => "INTEGER(GLenum)",
QuadricObj => "TYPE(C_PTR)",
SelectName => "INTEGER(GLuint)",
SizeI     => "INTEGER(GLsizei)",
StencilValue => "INTEGER(GLint)",
String    => "CHARACTER(C_CHAR), DIMENSION(*)",
TesselatorObj => "TYPE(C_PTR)",
Texture => "INTEGER(GLuint)",
TextureComponentCount => "INTEGER(GLint)",
UInt8     => "INTEGER(GLbyte)",
UInt16    => "INTEGER(GLshort)",
UInt32    => "INTEGER(GLint)",
WinCoord  => "INTEGER(GLint)",
void      => "TYPE(C_PTR)",
Void      => "TYPE(C_PTR)",
VoidPointer => "TYPE(C_PTR)",
);

if ($glu) {
  $enumfile="enumglu.spec";
}
else {
  $enumfile="enum.spec";
}
open (ENUM,"$path/$enumfile") or die "Can't open $path/$enumfile";
$enumtype="";
while (<ENUM>) {
  next if /^\s*$/ or /^\s*#/;  #  Blank line or comment
  next if /^\s*use /;
  if ( /^(\w+?)(SGI|EXT)?\s+enum:/ ) {
    $enumtype=$1;
    $comment=""; $comment= " ($2)" if defined($2);
    print "!  $enumtype values$comment\n";
    if ( !defined($map{$enumtype}) ) {
      $map{$enumtype}="INTEGER(GLenum)";
    }
  }
  elsif ( /\s*(\w+)\s+= (\w+)/ ) {
    next unless $enumtype;
    ($enum,$value)=/\s*(\w+)\s+= (\w+)/;
    $asread=$value;
    if ( $value=~/^0x/ ) {
      #  Hex constant value
      if ( $bozinit && $map{$enumtype} ne "REAL(GLfloat)" ) {
	#  Convert to Fortran z'...' form
	if ($value=~/0xffffffff/i) {
	  #  (This is the only top-bit-set value used)
          $value="                &\n    transfer(z'ffffffff',GL_CURRENT_BIT)";
	  #  The second argument is an arbitrary GLbitfield parameter
	} else {
	  $value=~s/0x(\w+)/z'$1'/;
	}
      }
      else {
	#  Convert to decimal integer
	if ($value=~/0xffffffff/i) {
	  #  (This is the only top-bit-set value used)
          $value=-1;
	} else {
	  $value=oct($value);
	}
      }
    }
    if ( $map{$enumtype} eq "REAL(GLfloat)" ) {
      printf "%s, $PARAMETER :: %-24s = %s ! %s\n",
	    $map{$enumtype}, "${library}_${enum}", "transfer($value,1.0)", $asread;
    }
    else {
      printf "%s, $PARAMETER :: %-24s = %s ! %s\n",
	      $map{$enumtype}, "${library}_${enum}", $value, $asread;
    }
  }
  else {
    #  Unrecognised
    $enumtype="";
  }
}

if ($PRIVATE) {
  print "\n\n";
}
else {
print "

INTERFACE
";
}

open (STDIN,"$path/$prefix.spec") or die "Can't open $path/$prefix.spec";

while (<>) {
  next if /^\s*$/;
  if ( /^# *(.+) *commands$/ ) {
    #  Comment at head of command section
    print "!  $1 commands\n\n";
  }
  elsif ( /^\s*#/ ) {
    #  Comment
    next;
  }
  elsif ( /^required-props/ ) {
    #  Ignore until next blank line
    while (<>) {
      last if /^\s*$/;
    }
  }
  elsif ( /^\w+\([\w, ]*\)/ ) {
    #  This looks like a procedure specification
    print "!  $_";
    chomp;
    %declare=();
    ($procname,$args)=/^(\w+)\(([\w, ]*)\);?$/;
    $procname="$prefix$procname";
    while (<>) {
      if ( /^\s*$/ || eof ) {
	#  Blank line ends specification
	print $header;
	foreach (sort arrayslast keys %declare) {
	  print "$_ :: $declare{$_}\n";
	}
	print "END $proctype $procname\n\n";
        if($PRIVATE) {
           print "END INTERFACE\n\n";
        }
	last;
      }
      else {
	chomp;
	s/^\t+(\w+)\t+//;
	$type=$1;
	&handle_return($_), next if $type eq "return";
	&handle_param($_), next if $type eq "param";
	#  Ignore remaining items
	next;
      }
    }
  }
}

if(not $PRIVATE) {
   print "END INTERFACE\n";
}
if (not $glu) {
  if ($PRIVATE) {
    print "PUBLIC CString\n";
  }
  print "\n
CONTAINS

FUNCTION CString(string) RESULT(array)
  CHARACTER(LEN=*) :: string
  CHARACTER(KIND=C_CHAR), DIMENSION(len(string)+1) :: array

  INTEGER :: i

  do i=1, len(string)
    array(i)=string(i:i)
  end do
  array(len(string)+1)=c_null_char

END FUNCTION CString
";
}

print "
END MODULE  ${module}_${prefix}
";



sub handle_return {
  my $s=$_[0];
  print "!  return  $s\n";
  if ($s eq "void") {
    $proctype="SUBROUTINE";
    $return_type="";
  }
  else {
    $proctype="FUNCTION";
    ($type,$details)=$s=~/(\w+) *(.*)?/;
    #  any details ignored here
    if ( $type=~/string/i ) {
      $return_type="TYPE(C_PTR)";
    }
    else {
      if ( ! defined($map{$type}) ) {
	print "return type $type not defined\n";
	$map{$type}="*** $type";
      }
      $return_type=$map{$type};
    }
  }
  $header="";
  if($PRIVATE) {
     $header="PUBLIC :: $procname\nINTERFACE\n";
  }
  $header.=&split_line(qq+$proctype $procname($args) BIND(C,NAME="$procname")\n+);
  $header.="IMPORT\n";
  $header.="$return_type :: $procname\n" if $proctype eq "FUNCTION";
}


sub handle_param {
  my $s=$_[0];
  my $qualifiers="";
  my $intent="";
  my $retained=0;
  my $value=0;
  print "!  param   $s\n";
  ($variable,$type,$details)=$s=~/(\w+)\s+(\w+) +(.*)$/;
  if ( $details=~s/in +// ) {
    $intent=", INTENT(IN)";
  }
  if ( $details=~s/out +// ) {
    $intent=", INTENT(OUT)";
  }
  if ( $details=~s/value *// ) {
    $value=1 unless $type eq Float32Pointer || $type eq Float64Pointer;
  }
  if ( $details=~s/retained *// ) {
    $retained=1; # Should make it TYPE(C_PTR), VALUE
  }
  if ( $details=~s/reference *// ) {
    if ($type eq "void" || $type eq "Void") {
      $value=1;
    }
    else {
      $qualifiers.=", DIMENSION(*)";
      $value=0;
    }
  }
  if ( $details=~s/array *\[(\d+|\w+)\] *// ) {
    if ($type eq "void" || $type eq "Void") {
      $value=1;
    }
    elsif ( $1 ne "1" ) {
      $qualifiers.=", DIMENSION($1)";
      $value=0;
    }
  }
  if ( $details=~s/array *\[COMPSIZE\((.*)\)\] *//
       || $details=~s/array *\[.*?\] *// ) {
    if ($type eq "void" || $type eq "Void") {
      $value=1;
    }
    else {
      $qualifiers.=", DIMENSION(*)";
      $value=0;
    }
  }
  if ( $details!~/^ *$/ ) {
    if (! defined($unknown{$details}) ) {
      print "---> Parameter description term '$details' unknown\n";
      $unknown{$details}=1;
    }
  }

  if ($value) {
    $qualifiers.=", VALUE";
  }
  else {
    $qualifiers.=$intent;
  }

  if ( ! defined($map{$type}) ) {
    if ( $default ) {
      print STDERR "Assuming GLenum for parameter type $type in procedure $procname\n";
      $map{$type}="INTEGER(GLenum)";
    }
    else {
      print "---> Unrecognised parameter type $type\n";
      $map{$type}="*** $type";
    }
  }
  $param_type=$map{$type};
  # Retained values must be handled differently:
  if($retained) {
     $param_type="TYPE(C_PTR)";
     $qualifiers=", VALUE";
  }
  if ( defined($declare{"$param_type$qualifiers"}) ) {
    #  Add this variable to an existing declaration
    $declare{"$param_type$qualifiers"}.=", $variable";
  }
  else {
    #  New declaration
    $declare{"$param_type$qualifiers"}="$variable";
  }
}

sub arrayslast {
  #  Sort map keys so that arrays follow scalars, in case the array
  #  dimension is defined by one of the scalars.
  my $A=($a=~/DIMENSION/i);
  my $B=($b=~/DIMENSION/i);
  if ($A!=$B) {
    $A <=> $B;
  }
  else {
    $a cmp $b;
  }
}

sub split_line {
  #  If the line is too long, split it and insert Fortran continuation
  #  symbols.
  my $s=$_[0];
  my $lines="";
  my $pad="";
  while ( length($s)+length($pad)>76 ) {
    $pad="    ";
    $s=~s/^(.{1,76}) +//;
    $lines.=($1 . " "x(78-length($1)) . "&\n$pad");
  }
  $lines.=$s;
}


sub help {
  print qq+
Usage: $0 <options>

Options:
--help    Print this help text.

-p path   Path to directory containing spec files. Default is to use
          the current directory.

-gl       Construct the gl.f90 interface module from gl.spec (default))
-glu      Construct the glu.f90 interface module from glu.spec.

--module prefix
          Prefix for module names. Default is "f03gl".
          Output is to <prefix>_gl.f90 or <prefix>_glu.f90.

-d        Unrecognised procedure parameter types are assumed to be
          GLenum and a message is printed on STDERR. Otherwise a message
          appears in the output file to the effect that the parameter
          type wasn't recognised.

--bozinit Hexadecimal parameter values are left in hex but expressed in
          Fortran notation, i.e. z'...' instead of 0x... Otherwise they
          are converted to decimal.

--PRIVATE (default) Only the symbols defined in this module will be
          exported.
--PUBLIC  Everything in this module and in any modules called from it
          will be exported.
+;
}
