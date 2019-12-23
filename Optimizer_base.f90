module Optimizer_baseclass
    
    use Lib_Objective
    
    implicit none
    private
    
    public :: Optimizer_base   
    
    type,abstract :: Optimizer_base
        
        integer :: Problem_dim
        integer :: maxIteration , maxIterationAtSamePos
        real :: tolerance
        real *8 , dimension(:), allocatable :: design_variables
        class (objective_base), allocatable :: Objective
        
    contains
        procedure(construct),deferred :: construct
        procedure(destruct),deferred :: destruct 
        procedure(optimize),deferred :: optimize
        
    end type
    
    ! Abstract interfaces
    
    abstract interface
        subroutine construct(this,Problem_Dim)
            import Optimizer_base
            class(Optimizer_base) :: this
            integer :: Problem_Dim
        end subroutine
            
        subroutine destruct(this)
            import Optimizer_base
            class(Optimizer_base) :: this
        end subroutine

        subroutine optimize(this)
            import Optimizer_base
            class(Optimizer_base) :: this
        end subroutine
        
    end interface
    
    
    contains
   
    
    
    
end module