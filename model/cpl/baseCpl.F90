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
   
    !sparse middle av 
    type(AttrVect)  :: a2x_bx
    type(AttrVect)  :: a2x_cx
    type(AttrVect)  :: b2x_ax
    type(AttrVect)  :: b2x_cx
    type(AttrVect)  :: c2x_ax
    type(AttrVect)  :: c2x_bx
   
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
    !---
    !A in 0,1,gsize=8   B in 2,3,gsize=12   C in 2,3,gsize=16
    !cpl in 0,1,2,3
    !---

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
        call a_init_mct(my_proc, my_proc%modela_id, EClock, gsMap_aa, a2x_aa, x2a_aa, ierr)
    end if

    if(my_proc%iamin_modelb)then
        call b_init_mct(my_proc, my_proc%modelb_id, EClock, gsMap_bb, b2x_bb, x2b_bb, ierr)
    end if

    if(my_proc%iamin_modelc)then
        call c_init_mct(my_proc, my_proc%modelc_id, EClock, gsMap_cc, c2x_cc, x2c_cc, ierr)
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) '<<==== All Model Init Rank:', comm_rank, " Over ====>>"
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    if(my_proc%iamin_modela2cpl)then
        call gsmap_init_ext(my_proc, gsmap_aa, my_proc%modela_id, gsmap_ax, &
                            my_proc%cplid, my_proc%modela2cpl_id)


        call avect_init_ext(my_proc, a2x_aa, my_proc%modela_id, a2x_ax, &
                            my_proc%cplid, gsmap_ax, my_proc%modela2cpl_id)
        call avect_init_ext(my_proc, x2a_aa, my_proc%modela_id, x2a_ax, &
                            my_proc%cplid, gsmap_ax, my_proc%modela2cpl_id)
        
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
            write(*,*) '<<========ModelA2X Rank:', comm_rank,  &
            "ax_lsize:", avect_lsize(a2x_ax)," aa_lsize:", avect_lsize(a2x_aa),&
            ' ===========>>'
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)

        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
            write(*,*) '<<========ModelA2X Rank:', comm_rank,  'AV GSMap Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)

        call mapper_rearrsplit_init(my_proc%mapper_Ca2x, my_proc, gsmap_aa, my_proc%modela_id, &
                                     gsmap_ax, my_proc%cplid, my_proc%modela2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2a, my_proc, gsmap_ax, my_proc%cplid, &
                                     gsmap_aa, my_proc%modela_id, my_proc%modela2cpl_id, ierr)

        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
            write(*,*) '<<========ModelA2X Rank:', comm_rank,  'Rearrange Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
        
            write(*,*) ' '
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
        
        call mapper_comp_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, 100+10+1, ierr) 
    end if
    
    if(my_proc%iamin_modelb2cpl)then
        call gsmap_init_ext(my_proc, gsmap_bb, my_proc%modelb_id, gsmap_bx, &
                            my_proc%cplid, my_proc%modelb2cpl_id)

        call avect_init_ext(my_proc, b2x_bb, my_proc%modelb_id, b2x_bx, &
                          my_proc%cplid, gsmap_bx, my_proc%modelb2cpl_id)

        call avect_init_ext(my_proc, x2b_bb, my_proc%modelb_id, x2b_bx, &
                          my_proc%cplid, gsmap_bx, my_proc%modelb2cpl_id)

        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)
            write(*,*) '<<========Model_B2X Rank:', comm_rank,  &
            "bx_lsize:", avect_lsize(b2x_bx)," bb_lsize:", avect_lsize(b2x_bb),&
            ' ===========>>'
        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)

        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)
            write(*,*) '<<========Model_B2X Rank:', comm_rank,  'AV GSMap Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)


        call mapper_rearrsplit_init(my_proc%mapper_Cb2x, my_proc, gsmap_bb, my_proc%modelb_id, &
                                     gsmap_bx, my_proc%cplid, my_proc%modelb2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2b, my_proc, gsmap_bx, my_proc%cplid, &
                                     gsmap_bb, my_proc%modelb_id, my_proc%modelb2cpl_id, ierr)
        
        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)
            write(*,*) '<<========Model_B2X Rank:', comm_rank,  'Rearrange Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)
            write(*,*) ' '
        call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)

        call mapper_comp_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, 100+20+1, ierr)
    end if

    if(my_proc%iamin_modelc2cpl)then
        call gsmap_init_ext(my_proc, gsmap_cc, my_proc%modelc_id, gsmap_cx,  &
                            my_proc%cplid, my_proc%modelc2cpl_id)
        call avect_init_ext(my_proc, c2x_cc, my_proc%modelc_id, c2x_cx, &
                            my_proc%cplid, gsmap_cx, my_proc%modelc2cpl_id)
        call avect_init_ext(my_proc, x2c_cc, my_proc%modelc_id, x2c_cx, &
                            my_proc%cplid, gsmap_cx, my_proc%modelc2cpl_id)
        
        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)
            write(*,*) '<<========Model_C2X Rank:', comm_rank,  &
            "cx_lsize:", avect_lsize(c2x_cx)," cc_lsize:", avect_lsize(c2x_cc),&
            ' ===========>>'
        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)

        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)
            write(*,*) '<<========Model_C2X Rank:', comm_rank,  'AV GSMap Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)


        call mapper_rearrsplit_init(my_proc%mapper_Cc2x, my_proc, gsmap_cc, my_proc%modelc_id, &
                                     gsmap_cx, my_proc%cplid, my_proc%modelc2cpl_id, ierr)
        call mapper_rearrsplit_init(my_proc%mapper_Cx2c, my_proc, gsmap_cx, my_proc%cplid, &
                                     gsmap_cc, my_proc%modelc_id, my_proc%modelc2cpl_id, ierr)

        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)
            write(*,*) '<<========Model_C2X Rank:', comm_rank,  'Rearrange Init Over ===========>>'
        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)
            write(*,*) ' '
        call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)

        call mapper_comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, 100+30+1, ierr)
    end if 


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(6,*) '<<========= Rank:',comm_rank,' Model-XInit End====>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! Init a2x_bx,a2x_cx ...
    if(my_proc%iamin_cpl) then
        call avect_init_ext(my_proc, a2x_ax, my_proc%cplid, &
                            a2x_bx, my_proc%cplid, &
                            gsmap_bx, my_proc%modelb2cpl_id)

        call avect_init_ext(my_proc, a2x_ax, my_proc%cplid, &
                            a2x_cx, my_proc%cplid, &
                            gsmap_cx, my_proc%modelc2cpl_id)

        call avect_init_ext(my_proc, b2x_bx, my_proc%cplid, &
                            b2x_ax, my_proc%cplid, &
                            gsmap_ax, my_proc%modela2cpl_id)

        call avect_init_ext(my_proc, b2x_bx, my_proc%cplid, &
                            b2x_cx, my_proc%cplid, &
                            gsmap_cx, my_proc%modelc2cpl_id)

        call avect_init_ext(my_proc, c2x_cx, my_proc%cplid, &
                            c2x_ax, my_proc%cplid, &
                            gsmap_ax, my_proc%modela2cpl_id)

        call avect_init_ext(my_proc, c2x_cx, my_proc%cplid, &
                            c2x_bx, my_proc%cplid, &
                            gsmap_bx, my_proc%modelb2cpl_id)
    
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) "<<=== Rank:" , comm_rank, &
            " lb2x_ax:", avect_lsize(b2x_ax),&
            " lc2x_ax:", avect_lsize(c2x_ax),&
            " la2x_bx:", avect_lsize(a2x_bx),&
            " lc2x_bx:", avect_lsize(c2x_bx),&
            " la2x_cx:", avect_lsize(a2x_cx),&
            " lb2x_cx:", avect_lsize(b2x_cx),&
            "===>>"
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) " "
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        
        call mapper_spmat_init(my_proc, my_proc%mapper_SMata2b, &
                my_proc%cplid, &
                my_proc%b_gsize, my_proc%a_gsize, 8,&
                gsmap_ax, gsmap_bx)

        call mapper_spmat_init(my_proc, my_proc%mapper_SMata2c, &
                my_proc%cplid, &
                my_proc%c_gsize, my_proc%a_gsize, 8,&
                gsmap_ax, gsmap_cx)

        call mapper_spmat_init(my_proc, my_proc%mapper_SMatb2a, &
                my_proc%cplid, &
                my_proc%a_gsize, my_proc%b_gsize, 8,&
                gsmap_bx, gsmap_ax)

        call mapper_spmat_init(my_proc, my_proc%mapper_SMatb2c, &
                my_proc%cplid, &
                my_proc%c_gsize, my_proc%b_gsize, 8,&
                gsmap_bx, gsmap_cx)
        
        call mapper_spmat_init(my_proc, my_proc%mapper_SMatc2a, &
                my_proc%cplid, &
                my_proc%a_gsize, my_proc%c_gsize, 8,&
                gsmap_cx, gsmap_ax)

        call mapper_spmat_init(my_proc, my_proc%mapper_SMatc2b, &
                my_proc%cplid, &
                my_proc%b_gsize, my_proc%c_gsize, 8,&
                gsmap_cx, gsmap_bx)
    endif
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) '<<==== SMat Init Rank:', comm_rank, " Over ====>>"
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*) '<<============== Rank:',comm_rank,'All Init End==================>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer :: ierr,s,i,comm_rank
    call triger(EClock, stop_clock, "stop_clock") 
    call mpi_comm_rank(my_proc%comp_comm(my_proc%gloid), comm_rank, ierr)
    
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*) '<<============== Rank:',comm_rank,' Begin Run==================>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    s = 0
    
    do while (.not. stop_clock)

        call clock_advance(EClock)
        call triger(EClock, a_run, "a_run")
        call triger(EClock, b_run, "b_run") 
        call triger(EClock, c_run, "c_run")
        call triger(EClock, stop_clock, "stop_clock") 

        !----------------------------------------------------------
        !   prep phase
        !----------------------------------------------------------  
        s = s+1
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
       ! write(*,*) s, a_run, b_run, c_run
        call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)

        if(s==10) stop_clock = .true.

        
        !------------------------------------------------------------
        !  Run phase 1 X2M_MX --> X2M_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------
        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                if(s == 3) then
                    ! A First Run init x2a_ax
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif

                    call mapper_comp_map(my_proc%mapper_Cx2a, &
                    x2a_ax, x2a_aa, 100+10+2, ierr)
                if(s==3)then 
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                endif
            end if
        end if        

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                if(s == 7) then
                    ! B First Run init x2b_bx
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
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
        !  Run phase 2, Model Run,  X2M_MM --> M2X_MM
        !  (M is Model, X is CPL)
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
                    write(*,*)
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !-------------------------------------------------------------
        !  update phase
        !-------------------------------------------------------------
        
        !------------------------------------------------------------
        !  Run phase 3
        !  For each Model:
        !  Step1: Rearrange, M2X_MM --> M2X_MX
        !  Step2: SparseMul With Other Model, M2X_MX --> M2X_BX
        !  (M is Model, X is CPL, B is Another Model)
        !------------------------------------------------------------
        !  For example:
        ! rearrage(a2x_aa,b2x_bb,c2x_cc) => (a2x_ax,b2x_bx,c2x_cx)
        ! sparse(a2x_ax b2x_bx c2x_cx) =>
        ! (a2x_bx,a2x_cx) (b2x_cx,b2x_a2) (c2x_ax,c2x_bx)
        !
        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                call mapper_comp_map(my_proc%mapper_Ca2x, a2x_aa, a2x_ax, 100+10+3, ierr)
                call mapper_comp_map(my_proc%mapper_SMata2b, a2x_ax, a2x_bx, 100+10+3, ierr, "x")
                call mapper_comp_map(my_proc%mapper_SMata2c, a2x_ax, a2x_cx, 100+10+3, ierr, "y")

                if(s==3)then
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===A2X_AX_VALUE Rank:',comm_rank, a2x_ax%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*) '<<===A2X_BX_VALUE Rank:',comm_rank, a2x_bx%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*) '<<===A2X_CX_VALUE Rank:',comm_rank, a2x_cx%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                endif
                
            end if

        end if

        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call mapper_comp_map(my_proc%mapper_Cb2x, b2x_bb, b2x_bx, 100+20+3, ierr)
                
                call mapper_comp_map(my_proc%mapper_SMatb2a, b2x_bx, b2x_ax, 100+10+3, ierr, "x")
                call mapper_comp_map(my_proc%mapper_SMatb2c, b2x_bx, b2x_cx, 100+10+3, ierr, "y")

            if(s==7) then 
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*) '<<===B2X_BX_VALUE Rank:',comm_rank, b2x_bx%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*) '<<===B2X_AX_VALUE Rank:',comm_rank, b2x_ax%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
                    write(*,*) '<<===B2X_CX_VALUE Rank:',comm_rank, b2x_cx%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modelb2cpl_id), ierr)
            endif
            end if
        end if

        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call mapper_comp_map(my_proc%mapper_Cc2x, c2x_cc, c2x_cx, 100+30+3, ierr)
                call mapper_comp_map(my_proc%mapper_SMatc2a, c2x_cx, c2x_ax, 100+10+3, ierr, "x")
                call mapper_comp_map(my_proc%mapper_SMatc2b, c2x_cx, c2x_bx, 100+10+3, ierr, "y")

            end if
        end if
        
        !------------------------------------------------------------
        !  Run phase 4
        !  Merge (A2X_MX, B2X_MX, C2X_MX, M2X_MX)--> X2M_MX
        !  (M is Model, X is CPL, A,B,C is Another Model)
        !------------------------------------------------------------
        ! For Example:
        ! (c2x_ax,b2x_ax,a2x_ax) => (x2a_ax)
        ! (c2x_bx,b2x_bx,a2x_bx) => (x2b_bx)
        ! (c2x_cx,b2x_cx,a2x_cx) => (x2c_cx)
        if(s==10) then
            ! merge *2x_ax --> x2a_ax in rfield "x", cal the mean of all
            call mapper_comp_avMerge(a2x_ax, b2x_ax, c2x_ax, x2a_ax, "x")
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AX_Merge_VALUE Rank:',comm_rank, x2a_ax%rAttr(1,:)
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
        endif

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

