module baseCpl
use proc_def 
use comms_def
use procm, only: pm_init => init, clean
use comms
use timeM
use mct_mod
use mrg_mod
use fraction_mod
use comp_a
use comp_c
use comp_b
use comp_ocn
use comp_atm
use comp_lnd

     implicit none
     type(proc), target :: my_proc

    ! Declare gsMap of each Model
        type(gsMap) :: gsMap_aa
        type(gsMap) :: gsMap_ax
        type(gsMap) :: gsMap_cc
        type(gsMap) :: gsMap_cx
        type(gsMap) :: gsMap_bb
        type(gsMap) :: gsMap_bx
        type(gsMap) :: gsMap_ocnocn
        type(gsMap) :: gsMap_ocnx
        type(gsMap) :: gsMap_atmatm
        type(gsMap) :: gsMap_atmx
        type(gsMap) :: gsMap_lndlnd
        type(gsMap) :: gsMap_lndx

    ! Declare AttrVect of each Model(c2x_cx,c2x_cc,x2c_cx,x2c_cc)
        type(AttrVect),pointer ::a2x_aa
        type(AttrVect),pointer ::a2x_ax
        type(AttrVect),pointer ::x2a_aa
        type(AttrVect),pointer ::x2a_ax
        type(AttrVect),pointer ::c2x_cc
        type(AttrVect),pointer ::c2x_cx
        type(AttrVect),pointer ::x2c_cc
        type(AttrVect),pointer ::x2c_cx
        type(AttrVect),pointer ::b2x_bb
        type(AttrVect),pointer ::b2x_bx
        type(AttrVect),pointer ::x2b_bb
        type(AttrVect),pointer ::x2b_bx
        type(AttrVect),pointer ::ocn2x_ocnocn
        type(AttrVect),pointer ::ocn2x_ocnx
        type(AttrVect),pointer ::x2ocn_ocnocn
        type(AttrVect),pointer ::x2ocn_ocnx
        type(AttrVect),pointer ::atm2x_atmatm
        type(AttrVect),pointer ::atm2x_atmx
        type(AttrVect),pointer ::x2atm_atmatm
        type(AttrVect),pointer ::x2atm_atmx
        type(AttrVect),pointer ::lnd2x_lndlnd
        type(AttrVect),pointer ::lnd2x_lndx
        type(AttrVect),pointer ::x2lnd_lndlnd
        type(AttrVect),pointer ::x2lnd_lndx


    ! Declare Temp Merge AttrVect of each Model(m2x_nx)
         type(AttrVect):: a2x_bx
         type(AttrVect):: a2x_cx
         type(AttrVect):: c2x_bx
         type(AttrVect):: c2x_ax
         type(AttrVect):: b2x_cx
         type(AttrVect):: b2x_ax
         type(AttrVect):: ocn2x_atmx
         type(AttrVect):: atm2x_ocnx
         type(AttrVect):: lnd2x_atmx

    ! Declare Control Var
	 logical :: a_run
	 logical :: c_run
	 logical :: b_run
	 logical :: ocn_run
	 logical :: atm_run
	 logical :: lnd_run

    type(AttrVect) :: fraction_atm
    type(AttrVect) :: fraction_ocn
    
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
                c2x_cc=> my_proc%c2x_cc
                c2x_cx=> my_proc%c2x_cx
                x2c_cc=> my_proc%x2c_cc
                x2c_cx=> my_proc%x2c_cx
                b2x_bb=> my_proc%b2x_bb
                b2x_bx=> my_proc%b2x_bx
                x2b_bb=> my_proc%x2b_bb
                x2b_bx=> my_proc%x2b_bx
                ocn2x_ocnocn=> my_proc%ocn2x_ocnocn
                ocn2x_ocnx=> my_proc%ocn2x_ocnx
                x2ocn_ocnocn=> my_proc%x2ocn_ocnocn
                x2ocn_ocnx=> my_proc%x2ocn_ocnx
                atm2x_atmatm=> my_proc%atm2x_atmatm
                atm2x_atmx=> my_proc%atm2x_atmx
                x2atm_atmatm=> my_proc%x2atm_atmatm
                x2atm_atmx=> my_proc%x2atm_atmx
                lnd2x_lndlnd=> my_proc%lnd2x_lndlnd
                lnd2x_lndx=> my_proc%lnd2x_lndx
                x2lnd_lndlnd=> my_proc%x2lnd_lndlnd
                x2lnd_lndx=> my_proc%x2lnd_lndx

         call fraction_atm_init(my_proc, fraction_atm, my_proc, fraction_ocn, &
                )
         call fraction_ocn_init(my_proc, fraction_atm, my_proc, fraction_ocn, &
                )

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
,x2a_aa=x2a_aa&
)
                end if
                if(my_proc%iamin_modelc)then
                    call c_init_mct(EClock=EClock&
,ID=my_proc%modelc_id&
,c2x_cc=c2x_cc&
,gsMap_cc=gsMap_cc&
,ierr=ierr&
,my_proc=my_proc&
,x2c_cc=x2c_cc&
)
                end if
                if(my_proc%iamin_modelb)then
                    call b_init_mct(EClock=EClock&
,ID=my_proc%modelb_id&
,b2x_bb=b2x_bb&
,gsMap_bb=gsMap_bb&
,ierr=ierr&
,my_proc=my_proc&
,x2b_bb=x2b_bb&
)
                end if
                if(my_proc%iamin_modelocn)then
                    call ocn_init_mct(EClock=EClock&
,ID=my_proc%modelocn_id&
,gsMap_ocnocn=gsMap_ocnocn&
,ierr=ierr&
,my_proc=my_proc&
,ocn2x_ocnocn=ocn2x_ocnocn&
,x2ocn_ocnocn=x2ocn_ocnocn&
)
                end if
                if(my_proc%iamin_modelatm)then
                    call atm_init_mct(EClock=EClock&
,ID=my_proc%modelatm_id&
,atm2x_atmatm=atm2x_atmatm&
,gsMap_atmatm=gsMap_atmatm&
,ierr=ierr&
,my_proc=my_proc&
,x2atm_atmatm=x2atm_atmatm&
)
                end if
                if(my_proc%iamin_modellnd)then
                    call lnd_init_mct(EClock=EClock&
,ID=my_proc%modellnd_id&
,gsMap_lndlnd=gsMap_lndlnd&
,ierr=ierr&
,lnd2x_lndlnd=lnd2x_lndlnd&
,my_proc=my_proc&
,x2lnd_lndlnd=x2lnd_lndlnd&
)
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
                if(my_proc%iamin_modelc2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_cc, &
                                        my_proc%modelc_id, &
                                        gsMap_cx, my_proc%cplid, &
                                        my_proc%modelc2cpl_id )

                    call avect_init_ext(my_proc, c2x_cc,&
                                        my_proc%modelc_id, c2x_cx, &
                                        my_proc%cplid, gsMap_cx, &
                                        my_proc%modelc2cpl_id)

                    call avect_init_ext(my_proc, x2c_cc,&
                                        my_proc%modelc_id, x2c_cx, &
                                        my_proc%cplid, gsMap_cx, &
                                        my_proc%modelc2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Cc2x, &   
                                                my_proc, gsMap_cc, my_proc%modelc_id, &
                                                gsMap_cx, my_proc%cplid, &
                                                my_proc%modelc2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2c, &
                                                my_proc, gsMap_cx, my_proc%cplid, &
                                                gsMap_cc, my_proc%modelc_id, &
                                                my_proc%modelc2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modelc2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Cc2x, &
                                         c2x_cc, c2x_cx, 100+10+1, ierr)
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
                if(my_proc%iamin_modelocn2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_ocnocn, &
                                        my_proc%modelocn_id, &
                                        gsMap_ocnx, my_proc%cplid, &
                                        my_proc%modelocn2cpl_id )

                    call avect_init_ext(my_proc, ocn2x_ocnocn,&
                                        my_proc%modelocn_id, ocn2x_ocnx, &
                                        my_proc%cplid, gsMap_ocnx, &
                                        my_proc%modelocn2cpl_id)

                    call avect_init_ext(my_proc, x2ocn_ocnocn,&
                                        my_proc%modelocn_id, x2ocn_ocnx, &
                                        my_proc%cplid, gsMap_ocnx, &
                                        my_proc%modelocn2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Cocn2x, &   
                                                my_proc, gsMap_ocnocn, my_proc%modelocn_id, &
                                                gsMap_ocnx, my_proc%cplid, &
                                                my_proc%modelocn2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2ocn, &
                                                my_proc, gsMap_ocnx, my_proc%cplid, &
                                                gsMap_ocnocn, my_proc%modelocn_id, &
                                                my_proc%modelocn2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modelocn2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Cocn2x, &
                                         ocn2x_ocnocn, ocn2x_ocnx, 100+10+1, ierr)
                end if
                if(my_proc%iamin_modelatm2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_atmatm, &
                                        my_proc%modelatm_id, &
                                        gsMap_atmx, my_proc%cplid, &
                                        my_proc%modelatm2cpl_id )

                    call avect_init_ext(my_proc, atm2x_atmatm,&
                                        my_proc%modelatm_id, atm2x_atmx, &
                                        my_proc%cplid, gsMap_atmx, &
                                        my_proc%modelatm2cpl_id)

                    call avect_init_ext(my_proc, x2atm_atmatm,&
                                        my_proc%modelatm_id, x2atm_atmx, &
                                        my_proc%cplid, gsMap_atmx, &
                                        my_proc%modelatm2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Catm2x, &   
                                                my_proc, gsMap_atmatm, my_proc%modelatm_id, &
                                                gsMap_atmx, my_proc%cplid, &
                                                my_proc%modelatm2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2atm, &
                                                my_proc, gsMap_atmx, my_proc%cplid, &
                                                gsMap_atmatm, my_proc%modelatm_id, &
                                                my_proc%modelatm2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modelatm2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Catm2x, &
                                         atm2x_atmatm, atm2x_atmx, 100+10+1, ierr)
                end if
                if(my_proc%iamin_modellnd2cpl)then
                    call gsmap_init_ext(my_proc, gsMap_lndlnd, &
                                        my_proc%modellnd_id, &
                                        gsMap_lndx, my_proc%cplid, &
                                        my_proc%modellnd2cpl_id )

                    call avect_init_ext(my_proc, lnd2x_lndlnd,&
                                        my_proc%modellnd_id, lnd2x_lndx, &
                                        my_proc%cplid, gsMap_lndx, &
                                        my_proc%modellnd2cpl_id)

                    call avect_init_ext(my_proc, x2lnd_lndlnd,&
                                        my_proc%modellnd_id, x2lnd_lndx, &
                                        my_proc%cplid, gsMap_lndx, &
                                        my_proc%modellnd2cpl_id)
                    call mapper_rearrsplit_init(my_proc%mapper_Clnd2x, &   
                                                my_proc, gsMap_lndlnd, my_proc%modellnd_id, &
                                                gsMap_lndx, my_proc%cplid, &
                                                my_proc%modellnd2cpl_id, ierr)

                    call mapper_rearrsplit_init(my_proc%mapper_Cx2lnd, &
                                                my_proc, gsMap_lndx, my_proc%cplid, &
                                                gsMap_lndlnd, my_proc%modellnd_id, &
                                                my_proc%modellnd2cpl_id, ierr)

                    call MPI_Barrier(my_proc%mpi_modellnd2cpl, ierr)
                    call mapper_comp_map(my_proc%mapper_Clnd2x, &
                                         lnd2x_lndlnd, lnd2x_lndx, 100+10+1, ierr)
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
                               8,&
                               gsMap_ax, gsMap_bx)

        call avect_init_ext(my_proc, a2x_ax,&
                            my_proc%cplid, a2x_cx,&
                            my_proc%cplid, gsMap_cx, &
                            my_proc%modelc2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMata2c, &
                               my_proc%cplid, &
                               my_proc%c_gsize, my_proc%a_gsize, &
                               8,&
                               gsMap_ax, gsMap_cx)

        call avect_init_ext(my_proc, c2x_cx,&
                            my_proc%cplid, c2x_bx,&
                            my_proc%cplid, gsMap_bx, &
                            my_proc%modelb2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatc2b, &
                               my_proc%cplid, &
                               my_proc%b_gsize, my_proc%c_gsize, &
                               8,&
                               gsMap_cx, gsMap_bx)

        call avect_init_ext(my_proc, c2x_cx,&
                            my_proc%cplid, c2x_ax,&
                            my_proc%cplid, gsMap_ax, &
                            my_proc%modela2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatc2a, &
                               my_proc%cplid, &
                               my_proc%a_gsize, my_proc%c_gsize, &
                               8,&
                               gsMap_cx, gsMap_ax)

        call avect_init_ext(my_proc, b2x_bx,&
                            my_proc%cplid, b2x_cx,&
                            my_proc%cplid, gsMap_cx, &
                            my_proc%modelc2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatb2c, &
                               my_proc%cplid, &
                               my_proc%c_gsize, my_proc%b_gsize, &
                               8,&
                               gsMap_bx, gsMap_cx)

        call avect_init_ext(my_proc, b2x_bx,&
                            my_proc%cplid, b2x_ax,&
                            my_proc%cplid, gsMap_ax, &
                            my_proc%modela2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatb2a, &
                               my_proc%cplid, &
                               my_proc%a_gsize, my_proc%b_gsize, &
                               8,&
                               gsMap_bx, gsMap_ax)

        call avect_init_ext(my_proc, x2ocn_ocnx,&
                            my_proc%cplid, ocn2x_atmx,&
                            my_proc%cplid, gsMap_atmx, &
                            my_proc%modelatm2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatocn2atm, &
                               my_proc%cplid, &
                               my_proc%atm_gsize, my_proc%ocn_gsize, &
                               8,&
                               gsMap_ocnx, gsMap_atmx)

        call avect_init_ext(my_proc, atm2x_atmx,&
                            my_proc%cplid, atm2x_ocnx,&
                            my_proc%cplid, gsMap_ocnx, &
                            my_proc%modelocn2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_Smatatm2ocn, &
                               my_proc%cplid, &
                               my_proc%ocn_gsize, my_proc%atm_gsize, &
                               8,&
                               gsMap_atmx, gsMap_ocnx)

        call avect_init_ext(my_proc, lnd2x_lndx,&
                            my_proc%cplid, lnd2x_atmx,&
                            my_proc%cplid, gsMap_atmx, &
                            my_proc%modelatm2cpl_id)

        call mapper_spmat_init(my_proc,&
                               my_proc%mapper_SMatlnd2atm, &
                               my_proc%cplid, &
                               my_proc%atm_gsize, my_proc%lnd_gsize, &
                               8,&
                               gsMap_atmx, gsMap_atmx)

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
        call triger(EClock, c_run, "c_run")
        call triger(EClock, b_run, "b_run")
        call triger(EClock, ocn_run, "ocn_run")
        call triger(EClock, atm_run, "atm_run")
        call triger(EClock, lnd_run, "lnd_run")
        call triger(EClock, stop_clock, "stop_clock")
        s = s+1
        if(s==10) stop_clock = .true.



        !------------------------------------------------------------
        !  Run phase 1 X2M_MX --> X2M_MM
        !  (M is Model, X is CPL)
        !------------------------------------------------------------


        if(a_run)then
            if(my_proc%iamin_modela2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2a_aa&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2a&
,msgtag=100+00+2&
,src=x2a_ax&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2c_cc&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2c&
,msgtag=100+10+2&
,src=x2c_cx&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2b_bb&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2b&
,msgtag=100+20+2&
,src=x2b_bx&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        if(ocn_run)then
            if(my_proc%iamin_modelocn2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2ocn_ocnocn&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2ocn&
,msgtag=100+30+2&
,src=x2ocn_ocnx&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        if(atm_run)then
            if(my_proc%iamin_modelatm2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2atm_atmatm&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2atm&
,msgtag=100+40+2&
,src=x2atm_atmx&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if
        if(lnd_run)then
            if(my_proc%iamin_modellnd2cpl)then
                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    do i=1,avect_lsize(x2a_ax)
                        x2a_ax%rAttr(1,i) = x2a_ax%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                if(s == 7 .and. my_proc%iamin_modelb2cpl) then
                    do i=1,avect_lsize(x2b_bx)
                        x2b_bx%rAttr(1,i) = x2b_bx%rAttr(1,i) + (comm_rank+1)*10+i
                    enddo
                endif
                
                call mapper_comp_map(dst=x2lnd_lndlnd&
,ierr=ierr&
,mapper=my_proc%Mapper_Cx2lnd&
,msgtag=100+50+2&
,src=x2lnd_lndx&
)

                if(s == 3 .and. my_proc%iamin_modela2cpl) then
                    call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                    write(*,*) '<<===X2A_AA_VALUE Rank:',comm_rank, x2a_aa%rAttr(1,:)
                call MPI_Barrier(my_proc%comp_comm(my_proc%modela2cpl_id), ierr)
                end if
            end if
        end if

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
,x2a=x2a_aa&
)
            end if
        end if
        if(c_run)then
            if(my_proc%iamin_modelc)then
                call c_run_mct(EClock=EClock&
,ID=my_proc%modelc_id&
,c2x=c2x_cc&
,ierr=ierr&
,my_proc=my_proc&
,x2c=x2c_cc&
)
            end if
        end if
        if(b_run)then
            if(my_proc%iamin_modelb)then
                call b_run_mct(EClock=EClock&
,ID=my_proc%modelb_id&
,b2x=b2x_bb&
,ierr=ierr&
,my_proc=my_proc&
,x2b=x2b_bb&
)
            end if
        end if
        if(ocn_run)then
            if(my_proc%iamin_modelocn)then
                call ocn_run_mct(EClock=EClock&
,ID=my_proc%modelocn_id&
,ierr=ierr&
,my_proc=my_proc&
,ocn2x=ocn2x_ocnocn&
,x2ocn=x2ocn_ocnocn&
)
            end if
        end if
        if(atm_run)then
            if(my_proc%iamin_modelatm)then
                call atm_run_mct(EClock=EClock&
,ID=my_proc%modelatm_id&
,atm2x=atm2x_atmatm&
,ierr=ierr&
,my_proc=my_proc&
,x2atm=x2atm_atmatm&
)
            end if
        end if
        if(lnd_run)then
            if(my_proc%iamin_modellnd)then
                call lnd_run_mct(EClock=EClock&
,ID=my_proc%modellnd_id&
,ierr=ierr&
,lnd2x=lnd2x_lndlnd&
,my_proc=my_proc&
,x2lnd=x2lnd_lndlnd&
)
            end if
        end if

        call MPI_Barrier(MPI_COMM_WORLD, ierr)
                    write(*,*)
        call MPI_Barrier(MPI_COMM_WORLD, ierr)

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
,src=a2x_aa&
)
call mapper_comp_map(dst=a2x_bx&
,ierr=ierr&
,mapper=my_proc%mapper_SMata2b&
,msgtag=100+00+4&
,rList='velo:cal'&
,src=a2x_ax&
)
call mapper_comp_map(dst=a2x_cx&
,ierr=ierr&
,mapper=my_proc%mapper_SMata2c&
,msgtag=100+00+4&
,rList='velo:cal'&
,src=a2x_ax&
)
            end if
        end if
        if(c_run)then
            if(my_proc%iamin_modelc2cpl)then
                call mapper_comp_map(dst=c2x_cx&
,ierr=ierr&
,mapper=my_proc%Mapper_Cc2x&
,msgtag=100+10+3&
,src=c2x_cc&
)
call mapper_comp_map(dst=c2x_bx&
,ierr=ierr&
,mapper=my_proc%mapper_SMatc2b&
,msgtag=100+10+4&
,rList='velo:cal'&
,src=c2x_cx&
)
call mapper_comp_map(dst=c2x_ax&
,ierr=ierr&
,mapper=my_proc%mapper_SMatc2a&
,msgtag=100+10+4&
,rList='velo:cal'&
,src=c2x_cx&
)
            end if
        end if
        if(b_run)then
            if(my_proc%iamin_modelb2cpl)then
                call mapper_comp_map(dst=b2x_bx&
,ierr=ierr&
,mapper=my_proc%Mapper_Cb2x&
,msgtag=100+20+3&
,src=b2x_bb&
)
call mapper_comp_map(dst=b2x_cx&
,ierr=ierr&
,mapper=my_proc%mapper_SMatb2c&
,msgtag=100+20+4&
,rList='velo:cal'&
,src=b2x_bx&
)
call mapper_comp_map(dst=b2x_ax&
,ierr=ierr&
,mapper=my_proc%mapper_SMatb2a&
,msgtag=100+20+4&
,rList='velo:cal'&
,src=b2x_bx&
)
            end if
        end if
        if(ocn_run)then
            if(my_proc%iamin_modelocn2cpl)then
                call mapper_comp_map(dst=ocn2x_ocnx&
,ierr=ierr&
,mapper=my_proc%Mapper_Cocn2x&
,msgtag=100+30+3&
,src=ocn2x_ocnocn&
)
call mapper_comp_map(dst=ocn2x_atmx&
,ierr=ierr&
,mapper=my_proc%mapper_SMatocn2atm&
,msgtag=100+30+4&
,rList='velo:cal'&
,src=ocn2x_ocnx&
)
            end if
        end if
        if(atm_run)then
            if(my_proc%iamin_modelatm2cpl)then
                call mapper_comp_map(dst=atm2x_atmx&
,ierr=ierr&
,mapper=my_proc%Mapper_Catm2x&
,msgtag=100+40+3&
,src=atm2x_atmatm&
)
call mapper_comp_map(dst=atm2x_ocnx&
,ierr=ierr&
,mapper=my_proc%mapper_Smatatm2ocn&
,msgtag=100+40+4&
,rList='velo:cal'&
,src=atm2x_atmx&
)
            end if
        end if
        if(lnd_run)then
            if(my_proc%iamin_modellnd2cpl)then
                call mapper_comp_map(dst=lnd2x_lndx&
,ierr=ierr&
,mapper=my_proc%Mapper_Clnd2x&
,msgtag=100+50+3&
,src=lnd2x_lndlnd&
)
call mapper_comp_map(dst=lnd2x_atmx&
,ierr=ierr&
,mapper=my_proc%mapper_SMatlnd2atm&
,msgtag=100+50+4&
,rList='velo:cal'&
,src=lnd2x_lndx&
)
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
                call mrg_a(a2x_ax, b2x_ax, c2x_ax)
                call mrg_x2ocn(my_proc, x2ocn_ocnx, atm2x_ocnx, fraction_ocn, &
        )
                call mrg_x2atm(my_proc, x2atm_atmx, ocn2x_atmx, lnd2x_atmx, &
          fractions_atm)
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
    if(my_proc%iamin_modelc)then
        call c_final_mct()
    end if
    if(my_proc%iamin_modelb)then
        call b_final_mct()
    end if
    if(my_proc%iamin_modelocn)then
        call ocn_final_mct()
    end if
    if(my_proc%iamin_modelatm)then
        call atm_final_mct()
    end if
    if(my_proc%iamin_modellnd)then
        call lnd_final_mct()
    end if
    call clean(my_proc)

end subroutine cpl_final

end module baseCpl
