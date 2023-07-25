program main
  !! Synchronized coarray communication of greetings created on multiple images
  implicit none
  integer, parameter :: max_greeting_length=64, writer = 1
  integer image
  character(len=max_greeting_length) :: greeting[*]

  associate(me => this_image(), ni=>num_images())

    write(greeting,*) "Hello from image", me , "of", ni
    sync all

    if (me == writer) then
      do image = 1, ni
        print *,greeting[image]
      end do 
    end if

  end associate
end program
