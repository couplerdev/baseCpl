

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    Math and Computer Science Division, Argonne National Laboratory   !
!-----------------------------------------------------------------------
! CVS $Id: twocmp.seq.F90,v 1.6 2006-07-25 17:09:42 jacob Exp $
! CVS $Name:  $ 
!BOP -------------------------------------------------------------------
!
! !ROUTINE:  twocomponent.sequential
!  
!
! !DESCRIPTION:  Provide a simple example of using MCT to connect
! two components executing in sequence in a single executable.
!
! Data is passed between models by using input/output arguments
! in the run method.  Compare with twocmp.seqNB.F90
!
! !INTERFACE:
!
      program main
      use proc_def
      use comms
      use mct_mod
      implicit none

      include 'mpif.h'

      integer,parameter :: ngx = 4   ! points in x-direction
      integer,parameter :: ngy = 4   ! points in y-direction
      integer,dimension(:),pointer :: myids
      integer ::i,color
      integer,dimension(1) :: start1,length1,peloc1
      integer,dimension(:),pointer :: start2,length2
!-----------------------------------------------------------------------
!  The Main program. 
! We are implementing a single-executable, sequential-execution system.
! In this example, communication occurs through main using
! arguments.  Both components share the same processors.

      type(gsMap) :: gsMap_aa_i, gsMap_aa_o, gsMap_ax
      type(AttrVect) :: a2x_aa, a2x_ax
      type(gRearr) :: Rearr,Rearr_xa
      type(proc) my_proc
      ! control signal
      logical :: iamin_model_a, iamin_a2x, iam_root_ina
      integer :: model_comm, model2x_comm
      integer pid_in_a, a_procs, avlsize_in_a
      integer ier, world_procs, pid_in_world
      integer j,nsegm,status, a_comm_i, a_comm_o
!-----------------------------------------------------------------------

      call MPI_init(ier)

      call mpi_comm_rank(MPI_COMM_WORLD, pid_in_world, ier)
      call mpi_comm_size(MPI_COMM_WORLD, world_procs, ier)

      ! pre init control signal
      if (pid_in_world .lt. world_procs/2) then
        color = 0
        iamin_model_a = .true.
      else 
        color = 1
        iamin_model_a = .false.
      endif
      if (pid_in_world == 0) then
        iam_root_ina = .true.     
      else
        iam_root_ina = .false.
      endif
      iamin_a2x = .true.

      call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,0,model_comm,ier)
      call mpi_comm_dup(MPI_COMM_WORLD,model2x_comm,ier)


      allocate(myids(2))
      myids(1)=1
      myids(2)=2
      a_comm_i = 1
      a_comm_o = 2
        
        
      call mct_world_init(2,MPI_COMM_WORLD, model2x_comm,myids=myids)

!  set up a grid and decomposition
! first gsmap is the grid decomposed by rows
! theres 1 segment per processor

      ! MODEL A INIT
      ! init gsMap of model_a
      if (iamin_model_a) then
          call mpi_comm_size(model_comm, a_procs, ier)
          call mpi_comm_rank(model_comm, pid_in_a, ier)
          length1(1)= ngx * (ngy/ a_procs)
          start1(1) = pid_in_a * length1(1) + 1

          call MCT_GSMap_init(gsMap_aa_i, start1, length1,0, model_comm ,1)
          write(6,*)'gsmap1',pid_in_world,  pid_in_a, "start:", &
            start1, "length:", length1
      endif
      call MPI_Barrier(model2x_comm, ier)
      ! init av of model_a
      if (iamin_model_a) then
          call avect_init(a2x_aa,rList="field1:field2", &
            lsize=gsMap_lsize(gsMap_aa_i, model_comm))
      endif
      call MPI_Barrier(model2x_comm, ier)
      ! MODEL A INIT OVER


      ! MODEL_A2X INIT
      ! init gsmap_ax by gsmap_aa
      call gsmap_init_ext(pid_in_world, gsMap_aa_i, gsMap_ax,&
                        model_comm, model2x_comm, &
                        a_comm_o)
      
      j = gsMap_lsize(gsMap_ax, model2x_comm)
      do i=1, j
        write(6,*) 'gsmap2', pid_in_world, "segment id:", i,&
            "start:", gsMap_ax%start(i)," length:",gsMap_ax%length(i)
      call MPI_Barrier(model2x_comm, ier)
      enddo
      ! init a2x_aa whose lsize=0 if not in model_a, and a2x_ax whose
      ! grid is new
      call avect_init_ext(1, a2x_aa, a2x_ax, &
                        model_comm, model2x_comm, &
                        gsMap_ax, iamin_model_a)
      call MPI_Barrier(model2x_comm, ier)
      ! MODEL_A2X INIT OVER 
      
      
      
      ! INIT Rearranger
      ! create tmp join gsmap_aa_o to init rearranger
      call gsmap_extend(gsmap_aa_i, gsmap_aa_o, &
                      model_comm, model2x_comm, &
                      a_comm_o)
      call rearr_init(gsMap_aa_o, gsMap_ax, model2x_comm, Rearr)
      call rearr_init(gsMap_ax, gsMap_aa_o, model2x_comm,Rearr_xa)
!-------------end of initialization steps


      ! Start up model1 which fills av1 with data.
      ! Run model_a and transfer av_a of model_a to a2x_aa 
      if(iamin_model_a ) then
        call model1(model_comm, a2x_aa, pid_in_world)
      endif
      call MPI_Barrier(model2x_comm, ier)
      !  print out Av data
      do i=1,avect_lsize(a2x_aa)
         write(6,*) "model 1 av_data", pid_in_world, pid_in_a, &
            " segmentid:",i,a2x_aa%rAttr(1,i),a2x_aa%rAttr(2,i)
      enddo
      call MPI_Barrier(model2x_comm, ier)
      
      ! rearrange data from model1 so that model2 can use it.
      call rearrange(a2x_aa,a2x_ax,Rearr, Sum=.false.)
      call MPI_Barrier(model2x_comm, ier)
      ! pass data to model2 (which will print it out)
      call model2(model2x_comm, a2x_ax, pid_in_world)
      call MPI_Barrier(model2x_comm, ier, pid_in_world)
        
      call MCT_Rearrange(a2x_ax,a2x_aa,Rearr_xa)
      call MPI_Barrier(model2x_comm, ier, pid_in_world)      
      
      if(iamin_model_a) then
        do i=1,avect_lsize(a2x_aa)
          write(6,*) "model_1 av", pid_in_world, pid_in_a, &
            " segmentid:",i,a2x_aa%rAttr(1,i),a2x_aa%rAttr(2,i)
       enddo
        call MPI_Barrier(model_comm, ier, pid_in_world)      
      endif
      
      
! all done
      call mpi_finalize(ier)

      contains

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! !ROUTINE: 
      subroutine model1(comm1,mod1av,pid_in_world)   ! the first model

      implicit none

      integer :: comm1,mysize,ier,asize,myproc
      integer :: fieldindx,avsize,i,pid_in_world
      integer,dimension(1) :: start,length
      real,pointer :: testarray(:)
      
      type(gsMap) :: GSmap
      type(AttrVect) :: mod1av
!---------------------------

!  find local rank and size
      call mpi_comm_size(comm1,mysize,ier)
      call mpi_comm_rank(comm1,myproc,ier)


      avsize = avect_lsize(mod1av)
      write(6,*)"model 1 av size", avsize, " id", myproc

!  Fill Av with some data
!  fill first attribute the direct way
      fieldindx = avect_indexRA(mod1av,"field1")
      do i=1,avsize
        mod1av%rAttr(fieldindx,i) = float(i+ 10*myproc + pid_in_world &
*100)
       ! if(pid_in_world .gt. 1) then
       !     mod1av%rAttr(fieldindx,i) = 0
       !  endif
      enddo

!  fill second attribute using Av import function
      allocate(testarray(avsize))
      do i=1,avsize
        testarray(i)= cos((float(i+ 20*myproc)/24.) * 3.14)
        if(pid_in_world .gt. 1) &
        testarray(i) = float(0)
      enddo
      call MCT_AtrVt_importRA(mod1av,"field2",testarray)


      end subroutine model1

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! !ROUTINE: 
      subroutine model2(comm2,mod2av,pid_in_world)

      implicit none

      integer :: comm2,mysize,ier,asize,myproc
      integer :: i,pid_in_world
      type(AttrVect) :: mod2av 
!---------------------------

!  find local rank and size
      call mpi_comm_size(comm2,mysize,ier)
      call mpi_comm_rank(comm2,myproc,ier)
      asize = avect_lsize(mod2av)
      write(6,*)"model2 procs num ",mysize, " av_size", asize
      call MPI_Barrier(comm2, ier, pid_in_world)      

!  print out Av data
      do i=1,asize
        write(6,*) "model 2 data proc", pid_in_world,i,mod2av%rAttr(1,i),mod2av%rAttr(2,i)
        mod2av%rAttr(1,i)=mod2av%rAttr(1,i) + 1000*pid_in_world
        mod2av%rAttr(2,i)=mod2av%rAttr(2,i) + 1000*pid_in_world
      enddo


      end subroutine model2

    
      
      
      end program main
