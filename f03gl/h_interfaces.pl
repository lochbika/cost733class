#!/usr/bin/perl -w

#  Construct a file of Fortran interfaces for the GLUT parameters and
#  procedures, using the GLUT.h header file as the starting-point.

$verbose=1;
$private=1;
$infile="OpenGLUT.h";
$module="f03gl";
$bozinit=0;
$no_dimension=0;
$command_line="$0 ".join(" ",@ARGV);
while (@ARGV) {
  $flag=shift;
  $verbose=0, next if $flag eq "-q";
  $verbose=1, next if $flag eq "-v";
  $private=1, next if $flag eq "--private";
  $infile=shift, next if $flag eq "-i";
  $bozinit=1, next if $flag eq "--bozinit";
  $no_dimension=1, next if $flag eq "--scalar";
  $module=shift, next if $flag eq "-m" or $flag eq "--module";
  &help, exit if $flag eq "--help";
  print "$flag not recognised\n";
  &help; exit;
}

#  Lookup table of Fortran equivalents to C argument types
%map = (
int        => "INTEGER(GLint)",
short      => "INTEGER(C_SHORT)",
float      => "REAL(C_FLOAT)",
double     => "REAL(C_DOUBLE)",
"unsigned int" => "INTEGER(GLuint)",
"char"   =>  "CHARACTER(C_CHAR)",
"char**"  =>  "TYPE(C_PTR), INTENT(IN)",
"unsigned char" => "INTEGER(GLbyte)",
"unsigned char*" => "CHARACTER, DIMENSION(*)",
GLint      => "INTEGER(GLint)",
GLuint     => "INTEGER(GLuint)",
GLenum     => "INTEGER(GLenum)",
GLboolean  => "INTEGER(GLboolean)",
GLbitfield => "INTEGER(GLbitfield)",
GLbyte     => "INTEGER(GLbyte)",
GLubyte    => "INTEGER(GLubyte)",
GLshort    => "INTEGER(GLshort)",
GLushort   => "INTEGER(GLushort)",
GLsizei    => "INTEGER(GLsizei)",
GLfloat    => "REAL(GLfloat)",
GLdouble   => "REAL(GLdouble)",
GLclampf   => "REAL(GLclampf)",
GLclampd   => "REAL(GLclampd)",
"void*"   => "TYPE(C_PTR)",
"void(*)" => "TYPE(C_FUNPTR)",
GLUTproc => "TYPE(C_FUNPTR)"
);

if ($private) {
  $ACCESS="PRIVATE";
  $PARAMETER="PARAMETER, PUBLIC";
  $PUBLIC=", PUBLIC";
}
else {
  $ACCESS="PUBLIC";
  $PARAMETER="PARAMETER";
  $PUBLIC="";
}

open (STDIN,$infile) or die "Can't open $infile";
open (STDOUT,">${module}_glut.f90") or die "Can't open ${module}_glut.f90 for output";

print "MODULE ${module}_glut

!  Derived from $infile using
!  $command_line

USE ${module}_kinds
IMPLICIT NONE
$ACCESS

";

$contained_wrappers="";

while (<>) {
  if ( /^#ifdef __cplusplus/ ) {
    #  Strip everything up to #endif (crude!)
    while (<>) {
      last if /^#endif/;
    }
  }
  if ( /^\s*$/) {
    #  Copy blank lines
    print "\n";
  }
  elsif ( m+^/\* *(.*) *\*/$+ ) {
    #  Copy one-line comments in Fortran form
    print "!  $1\n";
  }
  elsif ( m+^/\*+ ) {
    while (<>) {
      #  Copy multi-line comments
      last if m+\*/+;
      s/^ *\* *//;
      print "!  "; print;
    }
  }
  elsif ( /^#define +(\w+)\s+(.+)$/ ) {
    #  Parameter values
    $name=$1;
    $value=$2;
    if ( $value =~ /^0x(\w*)/ ) {
      if ( $bozinit ) {
        #  Convert to Fortran z'...' form
        $value=~s/0x(\w+)/z'$1'/;
      }
      else {
        #  Convert to decimal integer
        $value=oct($value);
      }
      print "INTEGER(GLenum), $PARAMETER :: $name = $value\n";
    }
    elsif ( $value =~ /^(\w+)$/ )  {
      print "INTEGER(GLenum), $PARAMETER :: $name = $value\n";
    }
    else {
      #  Unknown #define
      print "! ???, $PUBLIC :: $name=$value\n";
      # print "TYPE(C_PTR), BIND(C), PROTECTED$PUBLIC :: $name\n";
    }
  }
  elsif ( s/^ *extern +// || s/^\w*API +// ) {
    #  Is it a procedure definition?
    if ( /\w+APIENTRY/) {
      #  Make sure we have the whole thing
      while ( !/;$/ ) {
	chomp;
	$_.=<>;
      }
      #  Pass to subroutine with initial "extern" or "GLUTAPI" or "OGAPI"
      #  or "FGAPI" keyword now stripped
      $depth=0;
      $callback_wrapper=0; # Automatically generate the wrapper for glut callback functions
      &procdef($_);
      print "\n";
    }
  }
  else {
    #  Ignore
  }
}

print "END INTERFACE\n\n" unless $private;

# Manual handling
print "
! Font variables in GLUT_fonts.c
TYPE(C_PTR), BIND(C), PUBLIC, PROTECTED :: GLUT_STROKE_ROMAN,         &
    GLUT_STROKE_MONO_ROMAN, GLUT_BITMAP_9_BY_15, GLUT_BITMAP_8_BY_13, &
    GLUT_BITMAP_TIMES_ROMAN_10, GLUT_BITMAP_TIMES_ROMAN_24,           &
    GLUT_BITMAP_HELVETICA_10, GLUT_BITMAP_HELVETICA_12,               &
    GLUT_BITMAP_HELVETICA_18

! A special callback function for compatibility with f90gl
TYPE(C_FUNPTR), PUBLIC, SAVE :: GLUT_NULL_FUNC=C_NULL_FUNPTR

CONTAINS

SUBROUTINE glutInit_f03()
  INTEGER(C_INT) :: argcp=1
  TYPE(C_PTR), DIMENSION(1), TARGET :: argv=C_NULL_PTR
  CHARACTER(C_CHAR), DIMENSION(1), TARGET :: empty_string=C_NULL_CHAR

  ! A hack
  INTERFACE
   SUBROUTINE SetNullFunc() BIND(C,NAME='SetNullFunc')
   END SUBROUTINE
  END INTERFACE  

  argv(1)=C_LOC(empty_string)
  CALL glutInit_gl(argcp, C_LOC(argv))
  
END SUBROUTINE

$contained_wrappers
";

print "
END MODULE ${module}_glut

";

sub procdef {
  #  Procedure definition
  my $c_defn=$_[0];
  %declare=();
  local %interface=();
  my $procname="";
  my $proctype="";
  my $wrapper=0; # No wrapper--direct interface
  my $my_args="";
  
  $depth++;
  # return if $c_defn=~/^void *\*\w+;/;
  $c_defn=~s/\w*APIENTRY *//;  #  remove OGAPIENTRY etc.
  #  $c_defn=~s+ */\*.*\*/++;
  $c_defn=~s/ *; *$//;  #  Remove trailing semicolon
  print "!  $c_defn";
  ($return,$star,$procname,$param_list)=($c_defn=~/^(\w+) *(\*?|\(\*\)) *(\w+) *\((.*)\)$/);
  print STDERR "\n$procname\($param_list\)\n" if $verbose;
  ($return_type,$proctype)=&handle_return($return,$star);
  &handle_params($param_list);
  $my_args=$args; # Save a local copy of the argument list
  
  print "PUBLIC $procname\n" if $depth==1;
  
  if ( $depth==1 ) {
     if ( $procname =~ /glut\w*Func/ && $procname !~ /glutForceJoystickFunc/ ) 
     {
       print STDERR "Generating wrapper for $procname!\n";
       $callback_wrapper=1;
       $wrapper=1;
     }
     elsif ( $procname =~ /glutInit\b/ ) {
       $wrapper=1; # We supply a manual wrapper for this one
     }
  }
  
  if($callback_wrapper and ($depth>1)) {
     $callback_name=$procname;
     print "TYPE(C_FUNPTR), VALUE :: $procname\n";
     $contained_wrappers.="INTERFACE\n";
     $contained_wrappers.="$proctype $procname($args) BIND(C)\nIMPORT\n";
  }  
  else
  {
     if($wrapper) {
       print "INTERFACE ${procname}\n"; # The generic wrapper
       print "MODULE PROCEDURE ${procname}_f03\n";
       print "END INTERFACE ${procname}\n";
       print "INTERFACE\n"; # The C GLUT function
       print "$proctype ${procname}_gl($args) ";    
     }
     else {
       print "INTERFACE\n";
       print "$proctype $procname($args) ";
     }
     if ( $depth==1 ) {
       print qq+BIND(C,NAME="$procname")\nIMPORT\n+;
     }
     else {
       print "BIND(C)\nIMPORT\n";
     }
  }  
  if($callback_wrapper and ($depth==1)) { $contained_wrappers.="$proctype ${procname}_f03($args)\n"; }
  
  
  if ($proctype eq "FUNCTION") {
     if (not ($callback_wrapper and ($depth>1))) {
        print "$return_type :: $procname\n" ; 
     }
     if($callback_wrapper) { $contained_wrappers.="$return_type :: $procname\n"; }
  }   
     
  foreach (sort keys %declare) {
    $vars=$declare{$_};
    if (/DIMENSION\(\*\)/) {
      $dummy=$_;
      if ( $no_dimension && !/^CHARACTER/ ) {
	s/, *DIMENSION\(\*\)//;
      }
      else {
	$dummy=~s/, *DIMENSION\(\*\)//;
      }
      print "! $dummy :: $vars\n";      
    }
    if (not ($callback_wrapper and ($depth>1))) { print "$_ :: $vars\n"; }
    if($callback_wrapper) { $contained_wrappers.="$_ :: $vars\n"; }
  }
  
  for (keys %interface) {
    # print STDERR "$interface{$_}\n" if $verbose;
    &procdef($interface{$_});
  }
  
  if ($callback_wrapper and ($depth>1)) {
    $contained_wrappers.="END $proctype ${procname}\n";
    $contained_wrappers.="END INTERFACE\n";
  }
  else
  {
     if($wrapper) {
       print "END $proctype ${procname}_gl\n";
     }
     else {
       print "END $proctype $procname\n";
     }
     print "END INTERFACE\n";
  }
    
  if($callback_wrapper and ($depth==1)) {
     $c_loc_args=$my_args;
     $c_loc_args=~s/$callback_name/C_FUNLOC($callback_name)/;
     $c_null_args=$my_args;
     $c_null_args=~s/$callback_name/C_NULL_FUNPTR/;
     $contained_wrappers.=
"OPTIONAL :: $callback_name
IF(PRESENT($callback_name)) THEN
   CALL ${procname}_gl($c_loc_args)
ELSE
   CALL ${procname}_gl($c_null_args)
END IF
END $proctype ${procname}_f03\n";
  }
  $depth--;
}

sub handle_return {
  my $s=$_[0];
  my $star=$_[1];
  $s=~s/ +$//;
  if ($s eq "void" && $star eq "") {
    $proctype="SUBROUTINE";
    $return_type="";
  }
  else {
    $proctype="FUNCTION";
    $type=$s.$star;
    if ( ! defined($map{$type}) ) {
      print STDERR "return type $type not defined\n";
      $map{$type}="*** $type";
    }
    $return_type=$map{$type};
    #  VALUE attribute not possible for function result
    $return_type=~s/, VALUE//;
  }
  ($return_type,$proctype);
}

sub handle_params {
  my $param_string=$_[0];
  my $variable="";
  my $qualifiers="";
  my $intent="";
  my $value=1;
  $args="";
  my $a=0;
  #  Cycle while $param_string non-blank
  while ( $param_string!~/^\s*$/ ) {
    $variable="";
    $qualifiers="";
    $intent="";
    $value=1;
    last if $param_string=~/^ *void *$/;  #  Subroutine with no parameters
    $type="";
    while ( 1 ) {
      #  Extract type part of parameter specification
      ($word,$param_string)=($param_string=~/(\w+)\b *(.*)/);
      if ( $word eq "const" ) {
	$intent=", INTENT(IN)";
	$value=0;
	next;
      }
      elsif ( $word eq "unsigned" ) {
	#  Append to type string and loop
	$type.="$word ";
	next;
      }
      #  Otherwise append to type string and proceed
      $type.=$word;
      last;
    }
    #  Look for * or ** or (*) or (* callback); strip off if present.
    if ( $param_string=~s/^(\*+|\(\*( callback)?\)) *//i ) {
      $tag=$1;
      if ( $tag eq "(*)" || $tag=~/\(\* *callback\)/i ) {
	#  (*) or (* callback) means an un-named procedure -- give it a name ...
	$variable="proc";
	#  Remember its name and return type
	$interface{$variable}="$type $variable";
	#  ... and attach its arguments
	$param_string=~s/(\([^\)]+\))//;
	$interface{$variable}.="$1\n";
	$type="";
	print STDERR "$interface{$variable}\n" if $verbose;
      }
      elsif ( $type eq "char" && $tag eq "**" || $type eq "unsigned char"
	      || $type eq "void" ) {
        #  Append to type string
	$value=0 unless $type eq "void";
	$type.="$1";
      }
      else {
	$qualifiers.=", DIMENSION(*)";
	$value=0;
      }
    }
    elsif ( $param_string=~s/^\(\w*(CALLBACK)? *\*(\w+)\)//i ) {
      #  Callback procedure; the word is its name
      $variable=$2;
      $interface{$variable}="$type $variable";
      #  Attach its arguments
      $param_string=~s/(\([^\)]+\))//;
      $interface{$variable}.="$1\n";
      $type="";
      print STDERR "$interface{$variable}\n" if $verbose;
    }
    #  The parameter name is the next word, unless we have already
    #  assigned a name
    if (  $variable eq "" ) {
      if ($param_string=~/^(\w+)(.*)/) {
	($variable,$param_string)=($param_string=~/^(\w+)(.*)/) if $variable eq "";
      }
      else {
	#  No parameter name provided (usually because this is an
	#  argument of a callback). Invent one.
	$a++;
	$variable="arg$a";
      }
    }
    #  If [dimen] follows, it is an array dimension
    if ( $param_string=~s/^\[(.*?)\]// ) {
      $qualifiers=", DIMENSION($1)";
      $value=0;
    }
    #  If a comma follows, this is not the last parameter. Extract
    #  the separator.
    if ( $param_string=~s/^, *// ) {
      $sep=", ";
    }
    else {
      $sep="";
    }

    if ($value) {
      $qualifiers.=", VALUE";
    }
    else {
      $qualifiers.=$intent;
    }

    #  Append the argument name and separator to the argument string
    $args.="$variable$sep";
    #  Get the Fortran declaration for this parameter type from the
    #  lookup table.
    if ( $type ) {
      if ( defined($map{$type}) ) {
	$param_type=$map{$type};
	if ( defined($declare{"$param_type$qualifiers"}) ) {
	  #  Add this variable to an existing declaration
	  $declare{"$param_type$qualifiers"}.=", $variable";
	}
	else {
	  #  New declaration
	  $declare{"$param_type$qualifiers"}="$variable";
	}
      }
      else {
	print STDERR "Parameter type $type unknown\n";
      }
    }
  }
}

sub help {
  print qq+
Usage: $0 [-v | -q] [-i inputfile] [-m module | --module module] [--bozinit] [--scalar]
Translate the inputfile (default OpenGLUT.h) into a Fortran 2003 module
file.

The -m or --module option gives the base name of the module file.
The default is f03gl, so that the full name becomes f03gl_glut.f90.

-v (verbose) gives more output, -q suppresses it.

--bozinit :
hexadecimal parameter values are left in hex
but expressed in Fortran BOZ notation, i.e. z'...' instead of 0x...
Otherwise they are converted to decimal. This is not the default, since
BOZ constants are not allowed in initialisation expressions in standard
Fortran 2003, but some compilers accept them.

--scalar:
C declarations such as "int* v" describe a pointer to an entity v
which may be a scalar or an array. In Fortran it is necessary to
specify which is required. The attribute "DIMENSION(*)" is provided
unless this flag is present. In either case the alternative form is
also provided but is commented out. Exception: char* is always
translated to "CHARACTER(C_CHAR), DIMENSION(*)".
+;
}
