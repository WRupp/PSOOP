!Essa e uma subrotina pra calcular usando PSO de  uma forma orientada a objeto.
!Ela foi feita pensando na pratica de orientacao a objeto e testar vantagens e desvantagens...
!A vantagem é que seria mais facil mais visual algumas operacoes, mas no geral acho que nao vale a pena
!A implementacao fica mais dificil em alguns pontos e por demais verboso tendo que chamar milhoes de objetos por ai
!Claro, a informacao segue junta no objeto e fica mais facil de retirar informacoes depois 
!Mas tem que ficar lembrando qual procedure e de qual classe e tudo mais. Enquanto algumas coisas podiam ser feitas
!apenas fazendo um vetor e deu pra bola. talvez inclusive fique mais lento, porem nao tenho dados para isso
!    Wagner 
!    
!    
    
    
module PSO_OOP_mod
    
    use Optimizer_baseclass
    
    implicit none
    
    !private
    
    !public :: PSO_OOP_opt 
    

     
    type Particle
        integer :: ID
        real *8 :: Current_Value , Historic_Best 
        real *8, dimension(:), allocatable :: Position , Historic_Best_Position
        real *8, dimension(:), allocatable :: Velocity
    contains 
        !procedure :: Update_Position
        !procedure :: Update_Velocity
        !procedure :: Update_self_best
    end type
    
    type Swarm        
        integer :: Population_Number , Best_ID
        real *8 :: Swarm_Mean_Value
        class(Particle),dimension(:),allocatable :: Particles

    contains
        procedure :: Create_Swarm
        !procedure :: Update_Swarm_Best
    end type

    type, extends(Optimizer_base) :: PSO_OOP_opt
        
        integer:: Population_Number , Proximity_Pop_Total
        type(Swarm) :: Swarm
        real *8 :: Inertia, Social, Cognitive, Inertia_Reduction_Factor
        real *8 :: Proximity_Pop_Factor 
        real *8 , dimension(:,:), allocatable :: Variable_Limits
                
    contains
        procedure :: construct => PSO_construct
        procedure :: destruct => PSO_destruct
        procedure :: optimize => PSO

        procedure :: Get_Variable_Limits
        procedure :: Get_PSO_Parameters
        procedure :: Initiate_Population
        
        procedure :: Evaluate_Swarm
        procedure :: Update_Swarm_Position
        procedure :: Update_Swarm_Velocity
        procedure :: CheckCriteria
        
    end type 
        


    
    
    
    contains
     
    ! -------------------------------------------------------------------------------------    
   
    subroutine PSO_construct(this,Problem_Dim)
        class(PSO_OOP_opt) :: this
        integer :: Problem_Dim
            
        this%Problem_Dim = Problem_dim
            
        allocate(this%Design_variables(this%Problem_Dim))         
        allocate(this%Variable_Limits(this%Problem_Dim,2))
        call this%get_variable_limits
        call this%get_PSO_parameters
        
        
        this%Population_Number = 30 ! trocar por uma sub
        this%Swarm%Population_Number = this%Population_Number ! trocar por uma sub
        call this%Swarm%Create_Swarm(this%Problem_Dim)
        
        
        call this%initiate_population(0)
        ! Poderia iniciar o swarm aqui
            
    end subroutine 
    
    subroutine PSO_destruct(this)
        class(PSO_OOP_opt) :: this
        deallocate(this%Design_variables)         
        deallocate(this%Variable_Limits)
        deallocate(this%Swarm%Particles)
    end subroutine
    
    subroutine PSO(this)
    class(PSO_OOP_opt) :: this
    integer :: Iteration
    logical ContinueCriteria
    real :: Start_Time, Finish_Time , Elapsed_Time
    
        Start_Time = 0.0d0; Finish_Time = 0.0d0 ; Elapsed_Time = 0.0d0
    
        Iteration = 1
        ContinueCriteria = .true.
       
        do while ( ContinueCriteria )
            
             call CPU_time(Start_Time)
            
                call this%Update_Swarm_Position
            
                call this%Evaluate_swarm
            
                call this%Update_Swarm_Velocity
            

        
            call printOnscreen(this,Iteration,Elapsed_Time)
            
            call this%CheckCriteria(Iteration,ContinueCriteria)
       
            iteration = iteration + 1
            !print stuff
    
        end do
    
    this%Design_Variables = this%Swarm%Particles( this%Swarm%Best_ID )%Historic_Best_Position
    
    end subroutine
    
    ! -------------------------------------------------------------------------------------

    subroutine initiate_population(this,CaseNumber)
    ! Initiates Velocity and Positions 
    ! Each case dictates how to initiate these values
    
    implicit none
    class(PSO_OOP_opt) :: this
    integer ::  i , j , CaseNumber
    real *8 :: dummy
    !real *8, dimension(Prob_Dim,2) :: Variable_Limits
    !character(len=*) :: Posfilename , Velfilename
    
    !! Essa rotina precisa ser repensada. Como faria pra pegar (ou nao) as posicoes e velocidades de um arquivo?
    
    
    select case(CaseNumber)
            
    case(1) ! Reads the population from file Velocity is started with zero
   
        stop 404 !call Read_Table(Posfilename, Pos, this%Problem_Dim)
  
        do i = 1 , this%Population_Number
            do j = 1 , this%Problem_Dim
            this%Swarm%Particles(i)%Velocity(j) = 0.0d0;
            end do 
        end do
     
    case(2) ! Reads both position and velocity from files
   ! Reading velocity will be interesting in the case of restarting the PSO from a saved condition.
        
        stop 404 !call Read_Table(Posfilename, Pos, Prob_Dim) 
        
        stop 404 !call Read_Table(Velfilename, Velocity, Prob_Dim)
  
    case(3)
        
        ! this case is not implemented fully. It should take values ranging from 0 to 1 and then
        ! converted analogous region from the variable limits.
        
        ! Values must be converted for the correct limits for each of the variables
        ! Each line of the file is one individual. Each column is one position 
        
        ! Velocities are initiated with zero
        
        stop 404 !call Read_Table(Posfilename, Pos, Prob_Dim)
  
        do i = 1 , this%Population_Number
            do j = 1 , this%Problem_Dim
            this%Swarm%Particles(i)%Position(j) = ( this%Variable_Limits(j,2) - this%Variable_Limits(j,1) ) * this%Swarm%Particles(i)%Position(j) + this%Variable_Limits(j,1);
            this%Swarm%Particles(i)%Velocity(j) = 0.0d0;
            end do 
        end do
    
    case default ! Random
            
        do i = 1 , this%Population_Number
            do j = 1, this%Problem_Dim
                 call RANDOM_NUMBER(dummy)
                 this%Swarm%Particles(i)%Position(j) = this%Variable_Limits(j,1) + (this%Variable_Limits(j,2) - this%Variable_Limits(j,1)) * dummy
                 this%Swarm%Particles(i)%Velocity(j) = 0.0d0;
            end do
        end do
        
    end select
    
    end subroutine
       
    subroutine get_variable_limits(this)
    class(PSO_OOP_opt) :: this
    
    print *, 'Variable limits procedure not fully functional'
    ! Tem que criar esse procedure
    
    this%Variable_Limits(1,1) = 0.0d0 ; this%Variable_Limits(1,2) = 2.0d0 ; 
    this%Variable_Limits(2,1) = 0.0d0 ; this%Variable_Limits(2,2) = 2.0d0 ; 
    this%Variable_Limits(3,1) = 700.0d0 ; this%Variable_Limits(3,2) = 900.0d0 ; 
    
    !stop 404
    
    end subroutine
    
    subroutine get_PSO_parameters(this)
    class(PSO_OOP_opt) :: this
    
    this%Population_Number = 40 ;
        
    this%Inertia = 1.0 ; this%Inertia_Reduction_Factor = 0.99 ;
    this%Social = 2.0 ; this%Cognitive = 2.0 ;
    
    this%tolerance = 1.0D-3
    this%maxIteration = 10;
    this%maxIterationAtSamePos = 4;
    
    ! Scaled Proximity Variables
    this%Proximity_Pop_Factor = 0.8d0
    this%Proximity_Pop_Total = ceiling(this%Proximity_Pop_Factor * this%Population_Number)
      
    end subroutine
    
    ! -------------------------------------------------------------------------------------
        
    subroutine Create_Swarm(this,Problem_Dim)
        class(Swarm) :: this
        real *8 :: infinity
        integer :: Problem_Dim, i
        
        infinity = -log(0.0)
        
        allocate(this%Particles(this%Population_Number))
        
        do i = 1 , this%Population_Number
            this%Particles(i)%ID = i
            this%Particles(i)%Current_Value = infinity
            this%Particles(i)%Historic_Best = infinity
            allocate(this%Particles(i)%Position(Problem_Dim))
            allocate(this%Particles(i)%Historic_Best_Position(Problem_Dim))
            allocate(this%Particles(i)%Velocity(Problem_Dim))            
        end do
    
    end subroutine
    
    subroutine Evaluate_Swarm(this)
        class(PSO_OOP_opt) :: this
        integer :: i , Valid_Particles
        real *8 :: Valid_Particle_Sum
        
        Valid_Particles = 0
        Valid_Particle_sum = 0.0d0
        
        do i = 1 , this%Population_Number
           ! Evaluates Objective function 
           this%Swarm%Particles(i)%Current_Value = this%objective%evaluate( this%Swarm%Particles(i)%Position )
           
           ! Updates the particle history
           if ( this%Swarm%Particles(i)%Current_Value < this%Swarm%Particles(i)%Historic_Best ) then
               
                this%Swarm%Particles(i)%Historic_Best = this%Swarm%Particles(i)%Current_Value
                this%Swarm%Particles(i)%Historic_Best_Position = this%Swarm%Particles(i)%Position
    
           end if
           
           ! Get the swarm mean value at this iteration
           if ( this%Swarm%Particles(i)%Current_Value < 1.0d300 ) then ! hard coded 
           Valid_Particle_Sum = Valid_Particle_Sum + this%Swarm%Particles(i)%Current_Value
           Valid_Particles = Valid_Particles + 1
           end if
           
        end do 
      
        this%Swarm%Best_ID = minloc(this%Swarm%Particles(:)%Historic_Best , 1 )  
        
        this%Swarm%Swarm_Mean_Value = Valid_Particle_Sum / real (Valid_Particles)
        
    end subroutine
    
    subroutine Update_Swarm_Velocity(this)
    class(PSO_OOP_opt) :: this
    integer :: i ,j 
    real *8 :: dummy , U_cognitive, U_social
    ! Updates the position and velocity
    
    this%Inertia = this%Inertia * this%Inertia_Reduction_Factor
    
    call RANDOM_NUMBER(dummy) ;
    U_cognitive = this%Cognitive * dummy
        
    call RANDOM_NUMBER(dummy) ;
    U_social = this%social * dummy
    
    do i = 1 , this%Population_Number
        do j = 1 , this%Problem_Dim
                
            ! Update Velocity Vector
            this%Swarm%Particles(i)%Velocity(j) = this%Inertia * this%Swarm%Particles(i)%Velocity(j) &
                                                + U_cognitive * ( this%Swarm%Particles( i )%Historic_Best_Position(j) - this%Swarm%Particles( i )%Position(j)  )  &
                                                + U_social  * ( this%Swarm%Particles( this%Swarm%Best_ID )%Historic_Best_Position(j) -  this%Swarm%Particles(i)%Position(j) )      
        end do
    end do

    end subroutine
    
    subroutine Update_Swarm_Position(this)
    class(PSO_OOP_opt) :: this
    integer :: i ,j 
    ! Updates the position and velocity
    
    do i = 1 , this%Population_Number
        do j = 1 , this%Problem_Dim            
            ! Update Position Vector
            this%Swarm%Particles(i)%Position(j) = this%Swarm%Particles(i)%Position(j) + this%Swarm%Particles(i)%Velocity(j)
            
            ! Return to search space		
            if (this%Swarm%Particles(i)%Position(j) < this%Variable_Limits(j,1) ) then
                this%Swarm%Particles(i)%Position(j) = this%Variable_Limits(j,1)
			elseif (this%Swarm%Particles(i)%Position(j) > this%Variable_Limits(j,2) ) then
                this%Swarm%Particles(i)%Position(j) = this%Variable_Limits(j,2)
            end if
        end do
    end do

    end subroutine
    
    subroutine CheckCriteria(this,Iteration,ContinueCriteria)!IterationAtSame)
    class(PSO_OOP_opt) :: this 
    integer :: i , j , Particle_counter , Dim_counter , Iteration !, IterationAtSame
    logical :: ContinueCriteria
    real *8 :: DistanceFromBest
    
    !!! Proximity Criteria - Verificar pq acho que ta contando por dimensao
    Particle_Counter = 0
    
    do i = 1 , this%Population_Number
            
        Dim_Counter  = 0
            
        do j = 1 , this%Problem_Dim
            ! Verifica particulas estao proximas do Best    
            DistanceFromBest = abs( this%Swarm%Particles( this%Swarm%Best_ID )%Historic_Best_Position(j) - this%Swarm%Particles(i)%Position(j) )     
            if (  DistanceFromBest < this%tolerance ) then   
                Dim_Counter = Dim_Counter + 1
            end if               
        end do
            
        if ( Dim_Counter == this%Problem_Dim ) then
        Particle_Counter = Particle_Counter + 1; 
        end if
            
    end do
        
    if (Particle_Counter >= this%Proximity_Pop_Total) then 
        print *, this%Proximity_Pop_Total , ' particles are in a range of ', this%Tolerance , 'of the known best'
        ContinueCriteria = .false.
    end if
    
    if ( Iteration >= this%MaxIteration) then
        print *, 'Maximum number of iterations reached. Stopping...'
        ContinueCriteria = .false.
    end if 
    
    end subroutine
    
    ! -------------------------------------------------------------------------------------    
    
    subroutine printOnscreen(this,Iteration,Elapsed_Time) !Iteration,Best_all_time,Best_at_Iteration,Pop_Mean,Elapsed_Time)
    class(PSO_OOP_opt) :: this
    integer :: Iteration
    real *8 :: Best_all_time, Pop_Mean,  Best_at_Iteration
    real ::Elapsed_Time 
    
        print *, '-------------------------------------------------------------' 
        print *, 'Iteration' , Iteration 
        print *, 'Best: ', this%Swarm%Particles( this%Swarm%Best_ID ) % Historic_Best  
        !print *, 'Best at iteration:' , Best_at_Iteration
        print *, 'Population mean: ' , this%Swarm%Swarm_Mean_Value
        print *, 'Elaspsed time: ' , Elapsed_Time , ' seconds'
        print *, '-------------------------------------------------------------' 
    
    end subroutine


    
end module