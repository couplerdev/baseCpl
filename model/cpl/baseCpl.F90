module baseCpl
use procm ,only: pm_init => init 
use cpl_vect
use timeM
    implicit none

    type(proc) :: my_proc

    type(mct_gsMap) :: gsMap_aa
    type(mct_gsMap) :: gsMap_ax
    type(mct_gsMap) :: gsMap_bb
    type(mct_gsMap) :: gsMap_bx
    type(mct_gsMap) :: gsMap_cc
    type(mct_gsMap) :: gsMap_cx
    public :: cpl_init
    public :: cpl_run
    public :: cpl_final
     
contains 

subroutine cpl_init()
    
    implicit none
    call init(my_proc)
    call init_clock(EClock)
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
        call avect_extend()
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
                call a_mct_run(my_proc, EClock, a2x_aa, x2a_aa, ierr) 
            end if
        end if

        if(b_run)then
            if(my_proc%iamin_modelb)then
                call b_mct_run(my_proc, EClock, b2x_bb, x2b_bb, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc)then
                call c_mct_run(my_proc, EClock, c2x_cc, x2c_cc, ierr)
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

