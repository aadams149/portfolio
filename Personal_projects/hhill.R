hhill <- function(names, populations, seats, min = 1, exclude = c()){
  #Required input:
  #names: a vector or dataframe column of states or other entities to be allocated seats
  #populations: the populations of each item in `names`
  #seats: the total number of seats to be allocated

  if(is.numeric(populations) == FALSE){
    tryCatch(
        populations = as.numeric(populations),
        error = function(e){
          message('An error occurred:\n', e)
        })}
      if(length(names) != length(populations)){
        errorCondition('Error: argument `names` must be the same length as argument `populations`')
      }
      else{
      #Bind the names and populations into a data frame
      df = cbind.data.frame(names, populations)
      #Drop DC since it's not a state
      df = df %>% filter(!(names %in% exclude))
      #Instantiate the seat count column
      df$seatcount = min
      #Calculate each state's initial seat score
      df$seatscore = df$populations/(sqrt(df$seatcount*(df$seatcount+1)))
      df$seat_number = 1:nrow(df)
      #Figure out how many seats are left to be allocated
      seats = seats - (nrow(df)*min)
      #Copy the dataframe for processing purposes
      df1 = df
      #New dataframe
      newdf = data.frame()
      #Loop 1: calculate which state gets which seat in which order
      for (i in 1:seats){
        maxrow = df1 %>%
          dplyr::filter(seatscore == max(seatscore))
        maxrow$seat_number = i+nrow(df)
        maxrow$seatcount = maxrow$seatcount+1
        others = df1 %>%
          dplyr::filter(seatscore != max(seatscore))
        newdf = rbind.data.frame(newdf, maxrow)
        df1 = rbind.data.frame(maxrow, others)
        df1$seatscore = df1$populations/(sqrt(df1$seatcount*(df1$seatcount+1)))
      }
      newdf = rbind.data.frame(df, newdf)
      #New dataframe
      newdf1 = data.frame()
      #Loop 2: Pull out the rows with the total seats allocated to each state
      for(i in unique(newdf$names)){
        state = df1 %>%
          dplyr::filter(names == i)
        toprow = state %>%
          filter(seatcount == max(seatcount))
        newdf1 = rbind.data.frame(newdf1, toprow)
      }
      #Create a vector of all states which did not receive more than 1 seat
      one_seat = newdf1 %>% filter(seatcount == 1) %>% select(names)
      #Add all items to list
      #seat_order = data frame of length seats-nrow(names), showing the order in which seats were allocated
      #one_seat = character vector of states which only get 1 seat
      #final_seats = data frame of length nrow(names), showing how many seats each state gets
      item_list = list(seat_order = newdf, one_seat = list(one_seat), final_seats = newdf1)
      item_list}
  }
