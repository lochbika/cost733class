bin_PROGRAMS = cost733class

cost733class_SOURCES = globvar.f90
cost733class_SOURCES += netcdfcheck.f90 
cost733class_SOURCES += netcdfinput.f90 
cost733class_SOURCES += writenetcdf.f90 
cost733class_LDADD = $(top_srcdir)/netcdf-4.0.1/libsrc/.libs/libnetcdf.a 

if GRIB
cost733class_SOURCES += gribcheck.f90 
cost733class_SOURCES += gribinput.f90 
cost733class_LDADD += $(top_srcdir)/grib_api-1.9.18/fortran/.libs/libgrib_api_f90.a
cost733class_LDADD += $(top_srcdir)/grib_api-1.9.18/src/.libs/libgrib_api.a
cost733class_LDADD += -lm -ljasper
AM_FCFLAGS += -I$(top_srcdir)/grib_api-1.9.18/fortran
else
cost733class_SOURCES += gribcheck-dummy.f90 
cost733class_SOURCES += gribinput-dummy.f90 
endif

cost733class_SOURCES += datainput.f90 clasinput.f90 \
days4mon.f90 nobs4dates.f90 scan_matfile.f90 \
finish.f90 subfunctions.f90 gaussfilter.f90 pca.f90 \
distfunc.f90 newseed.f90 sortcla.f90 numday.f90 \
percentile.f90 sort.f90 selectgrid.f90 \
list4dates.f90 coef4pcaproj.f90 geofunctions.f90 listcount.f90 \
centroids.f90 distancevect.f90 dist_ratio.f90 \
arguments.f90 dataviewinit.f90 clasoutput.f90 centoutput.f90 \
binclass.f90 jenkcoll.f90 wlk.f90 lit.f90 prototype.f90 \
tmodpca.f90 tmodpcat.f90 kruiz.f90 pcaxtr.f90 \
erpicum.f90 lund.f90 kirchhofer.f90 \
kmeans.f90 ckmeans.f90 dkmeans.f90 mkmeans.f90 kmedoids.f90 sandra.f90 som.f90 mixturemodel.f90 hcluster.f90 \
hclustdiv.f90 randommedoid.f90 randomclass.f90 \
assign.f90 substitute.f90 ecv.f90 brier.f90 \
aggregate.f90 correlate.f90 frequencies.f90 prognosis.f90 \
cpart.f90 drat.f90 ev_pf.f90 fsil.f90 sil.f90 wsd_cim.f90 \
compare_randindex.f90 aritest.f90 compare_patterns.f90 compare_timeseries.f90 \
plot_pixmap.f90 \
modttest.f90 effectivesamplesize.f90 \
main.f90
