module baseCpl
use proc_def
use comms_def
use procm ,only: pm_init => init , clean
use comms
use timeM
use mct_mod
use comp_a
use comp_b
!use comp_b
use comp_c
!use mpi

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
    logical :: a_run
    logical :: b_run
    logical :: c_run
    logical :: stop_clock
    type(clock) :: EClock

    public :: cpl_init
    public :: cpl_run
    public :: cpl_final


     
contains 

subroutine cpl_init()
    
    implicit none
    integer :: ierr
    integer :: comm_rank
    call pm_init(my_proc)
    call clock_init(EClock)
    
    !-----------------------------------------------------------------
    !  variables comp2x_yy point to relative my_proc%comp2x_yy
    !-----------------------------------------------------------------
    !write(*,*)'init begin'
    !call MPI_Barrier(MPI_COMM_WORLD, ierr)
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
    call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, ierr)
    if(my_proc%iamin_modela)then
        write(*,*)'a init: Im:', comm_rank
        call a_init_mct(my_proc, my_proc%modela_id, EClock, gsMap_aa, a2x_aa, x2a_aa, ierr)
    end if

    if(my_proc%iamin_modelb)then
        call b_init_mct(my_proc, my_proc%modelb_id, EClock, gsMap_bb, b2x_bb, x2b_bb, ierr)
    end if

    if(my_proc%iamin_modelc)then
        call c_init_mct(my_proc, my_proc%modelc_id, EClock, gsMap_cc, c2x_cc, x2c_cc, ierr)
    end if

    write(*,*)'comp init finished'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if(my_proc%iamin_modela2cpl)then
        call gsmap_init_ext(my_proc, gsmap_aa, my_proc%modela_id, gsmap_ax, &
                            my_proc%cplid, my_proc%modela2cpl_id)
        write(*,*) 'gsmap_init_ext end'
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
        call avect_init_ext(my_proc, a2x_aa, my_proc%modela_id, a2x_ax, &
                            my_proc%cplid, gsmap_ax, my_proc%modela2cpl_id)
        call mapper_rearrsplit_init(my_proc%mapper_Ca2x, my_proc, gsmap_aa, my_proc%modela_id, &
                                     gsmap_ax, my_proc%cplid, my_proc%modela2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2a, my_proc, gsmap_ax, my_proc%cplid, &
                                     gsmap_aa, my_proc%modela_id, my_proc%modela2cpl_id, ierr)
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
        call mapper_comp_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, 100+10+1, ierr) 
    end if
    
    if(my_proc%iamin_modelb2cpl)then
        call gsmap_init_ext(my_proc, gsmap_bb, my_proc%modelb_id, gsmap_bx, &
                            my_proc%cplid, my_proc%modelb2cpl_id)
        call avect_init_ext(my_proc, b2x_bb, my_proc%modelb_id, b2x_bx, &
                          my_proc%cplid, gsmap_bx, my_proc%modelb2cpl_id)
        call mapper_rearrsplit_init(my_proc%mapper_Cb2x, my_proc, gsmap_bb, my_proc%modelb_id, &
                                     gsmap_bx, my_proc%cplid, my_proc%modelb2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2b, my_proc, gsmap_bx, my_proc%cplid, &
                                     gsmap_bb, my_proc%modelb_id, my_proc%modelb2cpl_id, ierr)
        call mapper_comp_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, 100+20+1, ierr)
    end if

    if(my_proc%iamin_modelc2cpl)then
        call gsmap_init_ext(my_proc, gsmap_cc, my_proc%modelc_id, gsmap_cx,  &
                            my_proc%cplid, my_proc%modelc2cpl_id)
        call avect_init_ext(my_proc, c2x_cc, my_proc%modelc_id, c2x_cx, &
                            my_proc%cplid, gsmap_cx, my_proc%modelc2cpl_id)
        call mapper_rearrsplit_init(my_proc%mapper_Cc2x, my_proc, gsmap_cc, my_proc%modelc_id, &
                                     gsmap_cx, my_proc%cplid, my_proc%modelc2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2c, my_proc, gsmap_cx, my_proc%cplid, &
                                     gsmap_cc, my_proc%modelc_id, my_proc%modelc2cpl_id, ierr)
        call mapper_comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, 100+30+1, ierr)
    end if 
    
    write(*,*)'init end'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer :: ierr
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
                call mapper_comp_map(my_proc%mapper_Cx2a, x2a_ax, x2a_aa, 100+10+2, ierr)
            end if
        end if        

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call mapper_comp_map(my_proc%mapper_Cx2b, x2b_bx, x2b_bb, 100+20+2, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call mapper_comp_map(my_proc%mapper_Cx2c, x2c_cx, x2c_cc, 100+30+2, ierr)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        
        !------------------------------------------------------------
        !  run phase
        !------------------------------------------------------------
        if(a_run)then
            if(my_proc%iamin_modela)then
                call a_run_mct(my_proc, my_proc%modela_id, EClock, a2x_aa, x2a_aa, ierr) 
            end if
        end if

        if(b_run)then
            if(my_proc%iamin_modelb)then
                call b_run_mct(my_proc, my_proc%modelb_id, EClock, b2x_bb, x2b_bb, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc)then
                call c_run_mct(my_proc, my_proc%modelc_id, EClock, c2x_cc, x2c_cc, ierr)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !-------------------------------------------------------------
        !  update phase
        !-------------------------------------------------------------

        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                call mapper_comp_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, 100+10+3, ierr)
            end if
        end if

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call mapper_comp_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, 100+20+3, ierr)
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call mapper_comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, 100+30+3, ierr)
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

