#
DEVTOP = ..
include ../configure.user
#
OBJS=    DA_Constants.o  DA_Define_Structures.o  module_obs.o \
          setup.o  plstn1.o  plstn2.o  plots.o  llxy.o hiresmap.o  rd_wr_mm5.o \
        prs2sig.o mdlobs1.o mdlobs0.o  bint.o     p1p2.o       oned.o \
         ttdrhq.o    anal.o   plhmp.o  fbint.o    foned.o   plotslab.o \
        wndbarb.o   plhwd.o  ffdduv.o  plot_skewt.o  \
	module_date.o inside_window.o \
	Map.o

SRCF=  \
          setup.f  plstn1.f   plstn2.f plots.f llxy.f hiresmap.f rd_wr_mm5.f \
        prs2sig.f mdlobs1.f mdlobs0.f  bint.f     p1p2.f      oned.f \
         ttdrhq.f    anal.f  plhmp.f  fbint.f    foned.f  plotslab.f \
        wndbarb.f   plhwd.f ffdduv.f     Map.f

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	$(OBJS); 
	(cd Skewt  ;  $(MAKE)) ;
	$(AR) $(LIBDIR)$(FDVARLIB) $(OBJS)
	($(MAKE) Map.exe);
	(mv Map.exe ../.);
#
Map.exe:
	$(FC) -o $@ $(OBJ) $(LDFLAGS) $(LIBDIR)$(FDVARLIB) $(LIBS)

module_obs.o:	DA_Read_Obs_Info.inc DA_Read_Obs.inc DA_Setup_Obs_Structures.inc

source:	$(SRCF)

clean:	
	(cd Skewt; $(MAKE) clean) ; \
	$(RM_CMD) "#"*
