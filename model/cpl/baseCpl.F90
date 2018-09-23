module baseCpl
use proc_def 
use comms_def
use procm, only: pm_init => init, clean
use comms
use timeM
use mct_mod
!use mrg_mod
!use fraction_mod
use comp_a
use comp_b

     implicit none
     type(proc), target :: my_proc

    ! Declare gsMap of each Model
        type(gsMap) :: gsMap_aa
        type(gsMap) :: gsMap_ax
        type(gsMap) :: gsMap_bb
        type(gsMap) :: gsMap_bx

    ! Declare AttrVect of each Model(c2x_cx,c2x_cc,x2c_cx,x2c_cc)
        type(AttrVect),pointer ::a2x_aa
        type(AttrVect),pointer ::a2x_ax
        type(AttrVect),pointer ::x2a_aa
        type(AttrVect),pointer ::x2a_ax
        type(AttrVect),pointer ::b2x_bb
        type(AttrVect),pointer ::b2x_bx
        type(AttrVect),pointer ::x2b_bb
        type(AttrVect),pointer ::x2b_bx


    ! Declare Temp Merge AttrVect of each Model(m2x_nx)
         type(AttrVect):: a2x_bx

    ! Declare Control Var
	 logical :: a_run
	 logical :: b_run

    
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
    
    !-------------------------------------------------------------------
    ! !A in 0,1,gsize=8   B in 2,3,gsize=12   C in 2,3,gsize=16
    ! !Cpl in 0,1,2,3
    !-------------------------------------------------------------------

    !-------------------------------------------------------------------
    !  !Define Model_AV_MM 
    !-------------------------------------------------------------------
    
                a2x_aa=> my_proc%a2x_aa
                a2x_ax=> my_proc%a2x_ax
                x2a_aa=> my_proc%x2a_aa
                x2a_ax=> my_proc%x2a_ax
                b2x_bb=> my_proc%b2x_bb
                b2x_bx=> my_proc%b2x_bx
                x2b_bb=> my_proc%x2b_bb
                x2b_bx=> my_proc%x2b_bx


    call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, ierr)


    !-------------------------------------------------------------------
    ! !Model Init
    !-------------------------------------------------------------------
                if(my_proc%iamin_modela)then
                    call a_init_mct(EClock=EClock&
                        ,ID=my_proc%modela_id&
                        ,a2x_aa=a2x_aa&
                        ,gsMap_aa=gsMap_aa&
                        ,ierr=ierr&
                        ,my_proc=my_proc&
                        ,x2a_aa=x2a_aa)
                end if
                if(my_proc%iamin_modelb)then
                    call b_init_mct(EClock=EClock&
                        ,ID=my_proc%modelb_id&
                        ,b2x_bb=b2x_bb&
                        ,gsMap_bb=gsMap_bb&
                        ,ierr=ierr&
                        ,my_proc=my_proc&
                        ,x2b_bb=x2b_bb)
                end if

    
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) '<<==== All Model Init Rank:', comm_rank, &
        " Over ====>>"
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    

    !-------------------------------------------------------------------
    ! !Model_X gsmap_ext av_ext
    !-------------------------------------------------------------------
                if(my_proc%iamin_modela2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_aa, &
                                        my_proc%modela_id, &
                                        gsMap_ax, my_proc%cplid, &
                                        my_proc%modela2cpl_id )

                    call avect_init_ext(my_proc, a2x_aa,&
                                        my_proc%modela_id, a2x_ax, &
                                        my_proc%cplid, gsMap_ax, &
                                        my_proc%modela2cpl_id)

                    call avect_init_ext(my_proc, x2a_aa,&
                                        my_proc%modela_id, x2a_ax, &
                                        my_proc%cplid, gsMap_ax, &
                                        my_proc%modela2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Ca2x, &   
                                                my_proc, gsMap_aa, my_proc%modela_id, &
                                                gsMap_ax, my_proc%cplid, &
                                                my_proc%modela2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2a, &
                                                my_proc, gsMap_ax, my_proc%cplid, &
                                                gsMap_aa, my_proc%modela_id, &
                                                my_proc%modela2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modela2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Ca2x, &
                                         a2x_aa, a2x_ax, 100+10+1, ierr)
                end if
                if(iamroot_a)then
                    write(*,*)'-------------a initiated-----------'
                end if
                if(my_proc%iamin_modelb2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_bb, &
                                        my_proc%modelb_id, &
                                        gsMap_bx, my_proc%cplid, &
                                        my_proc%modelb2cpl_id )

                    call avect_init_ext(my_proc, b2x_bb,&
                                        my_proc%modelb_id, b2x_bx, &
                                        my_proc%cplid, gsMap_bx, &
                                        my_proc%modelb2cpl_id)

                    call avect_init_ext(my_proc, x2b_bb,&
                                        my_proc%modelb_id, x2b_bx, &
                                        my_proc%cplid, gsMap_bx, &
                                        my_proc%modelb2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Cb2x, &   
                                                my_proc, gsMap_bb, my_proc%modelb_id, &
                                                gsMap_bx, my_proc%cplid, &
                                                my_proc%modelb2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2b, &
                                                my_proc, gsMap_bx, my_proc%cplid, &
                                                gsMap_bb, my_proc%modelb_id, &
                                                my_proc%modelb2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modelb2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Cb2x, &
                                         b2x_bb, b2x_bx, 100+10+1, ierr)
                end if
                if(iamroot_b)then
                    write(*,*)'-------------b initiated-----------'
                end if


    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(6,*) '<<========= Rank:',comm_rank,' Model-XInit End====>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)


    if(my_proc%iamin_cpl) then

        call avect_init_ext(my_proc, a2x_ax,&
                            my_proc%cplid, a2x_bx,&
                            my_proc%cplid, gsMap_bx, &
                            my_proc%modelb2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMata2b, &
                               my_proc%cplid, &
                               my_proc%b_gsize, my_proc%a_gsize, &
                               3,&
                               gsMap_bx, gsMap_bx)

 
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        write(*,*) " "
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
    end if
    write(*,*)'<========= Init End  ===========>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine cpl_init

subroutine cpl_run()

    implicit none
    integer :: ierr,s,i,comm_rank
    
    call mpi_comm_rank(my_proc%comp_comm(my_proc%gloid), comm_rank, ierr)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    write(*,*) '<<============== Rank:',comm_rank,' Begin Run==================>>'
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
            write(*,*) ' '
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    s = 0

    call triger(EClock, stop_clock, "stop_clock")
    do while(.not. stop_clock)

        call clock_advance(EClock)
        call triger(EClock, a_run, "a_run")
        call triger(EClock, b_run, "b_run")
        call triger(EClock, stop_clock, "stop_clock")
        s = s+1
        if(s==10) stop_clock = .true.



        !------------------------------------------------------------
        !  Run phase 1 X2M_MX --> X2M_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------



        call MPI_Barrier(MPI_COMM_WORLD, ierr)


        !------------------------------------------------------------
        !  Run phase 2, Model Run,  X2M_MM --> M2X_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------

        if(a_run)then
            if(my_proc%iamin_modela)then
                call a_run_mct(EClock=EClock&
                        ,ID=my_proc%modela_id&
                        ,a2x=a2x_aa&
                        ,ierr=ierr&
                        ,my_proc=my_proc&
                        ,x2a=x2a_aa)
            end if
        end if
        if(b_run)then
            if(my_proc%iamin_modelb)then
                call b_run_mct(EClock=EClock&
                        ,ID=my_proc%modelb_id&
                        ,b2x=b2x_bb&
                        ,ierr=ierr&
                        ,my_proc=my_proc&
                        ,x2b=x2b_bb)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        if(my_proc%iam_root) write(*,*)'----------------run phase2 end -----------------------'
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
                call mapper_comp_map(dst=a2x_ax&
                        ,ierr=ierr&
                        ,mapper=my_proc%Mapper_Ca2x&
                        ,msgtag=100+00+3&
                        ,src=a2x_aa)
call mapper_comp_map(dst=a2x_bx&
                        ,ierr=ierr&
                        ,mapper=my_proc%mapper_SMata2b&
                        ,msgtag=100+00+4&
                        ,rList='vel:hit'&
                        ,src=a2x_ax)
            end if
        end if
        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call mapper_comp_map(dst=b2x_bx&
                        ,ierr=ierr&
                        ,mapper=my_proc%Mapper_Cb2x&
                        ,msgtag=100+10+3&
                        ,src=b2x_bb)
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
    if(my_proc%iamin_cpl) then
        if(s==10) then
            ! merge *2x_ax --> x2a_ax in rfield "x", cal the mean of all
            !call mapper_comp_avMerge(a2x_ax, b2x_ax, c2x_ax, x2a_ax, "x")
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AX_Merge_VALUE Rank:',comm_rank, x2a_ax%rAttr(1,:)
            !for mgr_routine in merge_subroutines
            !    set func_str = mgr_routine.toString(mgr_routine.name, mgr_routine.argList)
            !    call {mgr_routine.toString(mgr_routine.name,mgr_routine.argList)}
            !end for
            call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
        endif
    endif

    end do

end subroutine cpl_run

subroutine cpl_final()

    implicit none

    !----------------------------------------------------------------------
    !     end component
    !----------------------------------------------------------------------
    if(my_proc%iamin_modela)then
        call a_final_mct()
    end if
    if(my_proc%iamin_modelb)then
        call b_final_mct()
    end if
    call clean(my_proc)

end subroutine cpl_final

end module baseCpl
