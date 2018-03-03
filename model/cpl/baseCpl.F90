module baseCpl
use procm ,only: pm_init => init 
use cpl_vect
use timeM
    implicit none

    type(proc), target :: my_proc  ! my proc manage all info needed in this process

    type(gsMap) :: gsMap_aa
    type(gsMap) :: gsMap_ax
    type(gsMap) :: gsMap_bb
    type(gsMap) :: gsMap_bx
    type(gsMap) :: gsMap_cc
    type(gsMap) :: gsMap_cx

    type(AttrVect),pointer  :: a2x_aa
    type(AttrVect),pointer  :: x2a_aa
    type(AttrVect),pointer  :: a2x_ax
    type(AttrVect),pointer  :: x2a_ax
    type(AttrVect),pointer  :: b2x_bb
    type(AttrVect),pointer  :: x2b_bb
    type(AttrVect),pointer  :: b2x_bx
    type(AttrVect),pointer  :: x2b_bx
    type(AttrVect),pointer  :: c2x_cc
    type(AttrVect),pointer  :: x2c_cc
    type(AttrVect),pointer  :: c2x_cx
    type(AttrVect),pointer  :: x2c_cx
   
    !type(map_mod)   :: mapper_Ca2x
    !type(map_mod)   :: mapper_Cb2x
    !type(map_mod)   :: mapper_Cc2x
    !type(map_mod)   :: mapper_Cx2a
    !type(map_mod)   :: mapper_Cx2b
    !type(map_mod)   :: mapper_Cx2c


    public :: cpl_init
    public :: cpl_run
    public :: cpl_final


     
contains 

subroutine cpl_init()
    
    implicit none
    call pm_init(my_proc)
    call clock_init(EClock)
    
    !-----------------------------------------------------------------
    !  variables comp2x_yy point to relative my_proc%comp2x_yy
    !-----------------------------------------------------------------
    a2x_aa => my_proc%a2x_aa
    a2x_ax => my_proc%a2x_ax
    x2a_aa => my_proc%x2a_aa
    x2a_ax => my_proc%x2a_ax
    b2x_bb => my_proc%b2x_bb
    b2x_bx => my_proc%b2x_bx
    x2b_bb => my_proc%x2b_bb
    x2b_bx => my_proc%x2b_bx
    c2x_cc => my_proc%c2x_cc
    c2x_cx => my_proc%c2x_cx
    x2c_cc => my_proc%x2c_cc
    x2c_cx => my_proc%x2c_cx

    !-----------------------------------------------------------------
    ! below defined a order of models, if proc seperated properly
    ! they have no order, but with generator, user can define any
    ! order they want
    !-----------------------------------------------------------------
    if(my_proc%iamin_modela)then
        call a_init_mct(my_proc, EClock, gsMap_aa, a2x_aa, x2a_aa, ierr)
    end if

    if(my_proc%iamin_modelb)then
        call b_init_mct(my_proc, EClock, gsMap_bb, b2x_bb, x2b_bb, ierr)
    end if

    if(my_proc%iamin_modelc)then
        call c_init_mct(my_proc, EClock, gsMap_cc, c2x_cc, x2c_cc, ierr)
    end if

    if(my_proc%iamin_modela2cpl)then
        call gsmap_extend()
        call avect_extend(a2x_aa, a2x_ax)
        call mapper_rearr_init(my_proc%mapper_Ca2x, gsmap_aa, gsmap_ax, ierr)
        call mapper_rearr_init(my_proc%mapper_Cx2a, gsmap_ax, gsmap_aa, ierr)
        call MPI_Barrier(my_proc%iamin_modela2cpl, ierr)
        call comp_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, msgtag=, ierr) 
    end if
    
    if(my_proc%iamin_modelb2cpl)then
        call gsmap_extend()
        call avect_extend()
        call mapper_rearr_init(my_proc%mapper_Cb2x, gsmap_bb, gsmap_bx, ierr)
        call mapper_rearr_init(my_proc%mapper_Cx2b, gsmap_bx, gsmap_bb, ierr)
        call comp_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, msgtag=, ierr)
    end if

    if(my_proc%iamin_modelc2cpl)then
        call gsmap_extend()
        call avect_extend()
        call mapper_rearr_init(my_proc%mapper_Cc2x, gsmap_cc, gsmap_cx, ierr)
        call mapper_rearr_init(my_proc%mapper_Cx2c, gsmap_cx, gsmap_cc, ierr)
        call comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, msgtag=, ierr)
    end if 

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    call triger(EClock, stop_clock, "stop_clock") 
    do while (.not. stop_clock)

        call clock_advance(EClock)
        call triger(EClock, a_run, "a_run")
        call triger(EClock, b_run, "b_run") 
        call triger(EClock, c_run, "c_run")
        call triger(EClock, stop_clock, "stop_clock") 

        !----------------------------------------------------------
        !   prep phase
        !----------------------------------------------------------  
        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                call comm_map(my_proc%mapper_Cx2a, x2a_ax, x2a_aa, msgtag=, ierr)
            end if
        end if        

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call comm_map(my_proc%mapper_Cx2b, x2b_bx, x2b_bb, msgtag=, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call comm_map(my_proc%mapper_Cx2c, x2c_cx, x2c_cc, msgtag=, ierr)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        
        !------------------------------------------------------------
        !  run phase
        !------------------------------------------------------------
        if(a_run)then
            if(my_proc%iamin_modela)then
                call a_run_mct(my_proc, EClock, a2x_aa, x2a_aa, ierr) 
            end if
        end if

        if(b_run)then
            if(my_proc%iamin_modelb)then
                call b_run_mct(my_proc, EClock, b2x_bb, x2b_bb, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc)then
                call c_run_mct(my_proc, EClock, c2x_cc, x2c_cc, ierr)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !-------------------------------------------------------------
        !  update phase
        !-------------------------------------------------------------

        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                call comm_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, msgtag=, ierr)
            end if
        end if

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call comm_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, msgtag=, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, msgtag=, ierr)
            end if
        end if

    end do 

end subroutine cpl_run


subroutine cpl_final()
    
    implicit none
    
    !--------------------------------------------------------
    !   end component
    !--------------------------------------------------------
    if(my_proc%iamin_modela)then
        call a_final_mct()
    end if
 
    if(my_proc%iamin_modelb)then
        call b_final_mct()
    end if
 
    if(my_proc%iamin_modelc)then
        call c_final_mct()
    end if
    
    call clean(my_proc) 
 
end subroutine cpl_final


end module baseCpl

