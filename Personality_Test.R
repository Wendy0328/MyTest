


Personality_test<-function(NAME){
  
  cat("Hello",NAME,"! Are you ready to find out your personality secret? \n ")
  x<-readline(prompt = "Please type YES if you are ready and of course type NO if you are not:")
  
  if(x=="NO"){
    cat("OK, that is a pity, but anyway, find me when you want to! \n")
  }
  
  else if(x=="YES"){
    cat("That is exciting! Let's begin! \nPlease answer the questions:\n")
    cat("Why didn't Aladdin use one wish to ask the lamp to give him 100 \nmore wishes when he was promised only three wishes by the lamp? \n")
    cat("Please choose the answer you most agree with: \n")
    cat("A That's a good idea, but Aladdin failed to think of it \nB Aladdin was embarrassed to make such a request, because it might make the princess think he was greedy \n")
    
    y<-readline(prompt = "Please choose A or B and type your choice:")
    
    if(y=="A"){
      cat("The second question:\nDo you think Jack and Rose can actually both lie on the board in the end of the Titanic movie to avoid death?\n")
      
      cat("Please choose the answer you agree with:\n")
      cat("A Yes\nB No")
      z<-readline(prompt = "Please choose A or B and type your choice:")
      if(z=="A"){
        cat("The third question: \nWhich do you think is more beautiful, sunrise or sunset?\n")
        cat("Please choose the answer you agree with:\n")
        cat("A Sunrise\nB Sunset")
        k<-readline(prompt = "Please choose A or B and type your choice:")
        if(k=="A"){
          cat("RESULT:\n  ",NAME,", you are good at thinking, have excellent questioning spirit and will put your ideas \ninto practice. You will go further and further on the road of achieving great success.")
        }
        else if(k=="B"){
          cat("RESULT:\n   You are a person with sharp mind and good at dialectical thinking. Your wisdom \nfrom inside to outside makes you very charming.")
        }
        else{
          cat("Answer not valid")
        }
          
      }
      else if(z=="B"){
        cat("The third question: \nWhich do you think is more beautiful, sunrise or sunset?\n")
        cat("Please choose the answer you agree with:\n")
        cat("A Sunrise\nB Sunset")
        k<-readline(prompt = "Please choose A or B and type your choice:")
        
        if(k=="A"){
          cat("RESULT:\n  ",NAME,", you are a smart and romantic person, you may give people a reliable, optimistic and enthusiastic \nimpression, which makes everyone likes you very much.")
        }
        else if(k=="B"){
          cat("RESULT:\n  ",NAME,", you are a smart and cool person, you are always clear headed, full of wisdom, and most \nimportant, you are always true to yourself and pursuing what you really want.")
        }
        else{
          cat("Answer not valid")
        }
        
        
      }
      else{
        cat("Answer not valid")
      }
    }
    else if(y=="B"){
      
      
      cat("The second question:\nDo you think Jack and Rose can actually both lie on the board in the end of the Titanic movie to avoid death?\n")
      
      cat("Please choose the answer you agree with:\n")
      cat("A Yes\nB No")
      z<-readline(prompt = "Please choose A or B and type your choice:")
      if(z=="A"){
        cat("The third question: \nWhich do you think is more beautiful, sunrise or sunset?\n")
        cat("Please choose the answer you agree with:\n")
        cat("A Sunrise\nB Sunset")
        k<-readline(prompt = "Please choose A or B and type your choice:")
        if(k=="A"){
          cat("RESULT:\n  ",NAME,", you have a questioning spirit, you are a hard-working and thorough person, and good \nat taking care of other people's emotions. You have very high EQ.")
        }
        else if(k=="B"){
          cat("RESULT:\n  ",NAME,", you are a careful and wise person, you are good at noticing problems and resolving conflicts. \nYou are calm, and good at controlling the overall situation, a natural leader.")
        }
        else{
          cat("Answer not valid")
        }
        
      }
      else if(z=="B"){
        cat("The third question: \nWhich do you think is more beautiful, sunrise or sunset?\n")
        cat("Please choose the answer you agree with:\n")
        cat("A Sunrise\nB Sunset")
        k<-readline(prompt = "Please choose A or B and type your choice:")
        
        if(k=="A"){
          cat("RESULT:\n  ",NAME,", you are a vigorous, diligent and optimistic person, any difficulty can't beat you. \nYou are always on the way of progressing.")
        }
        else if(k=="B"){
          cat("RESULT:\n  You are a person who is good at discovering the beauty in life and you are very romantic \nand charming. You are like an artist.")
        }
        else {
          cat("Answer not valid")
        }
        
        
      }
      else{
        cat("Answer not valid")
      }
      
      
    }
    else{
    cat("Answer not valid")
    }
    

  }
  else{
    cat("Answer not valid")
  }
}
Personality_test("Ella")


