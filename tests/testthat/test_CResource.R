library(metaheuR)
library(testthat)

context("CResource class testing")


# Object definitions ------------------------------------------------------

empty.cr<-new("CResource",time_total = NULL , time_remaining = NULL ,
              evaluations_total = NULL , evaluations_remaining = NULL , 
              iterations_total = NULL , iterations_remaining = NULL)

allone.cr<-new("CResource",time_total = 1 , time_remaining = 1 ,
               evaluations_total = 1 , evaluations_remaining = 1 , 
               iterations_total = 1 , iterations_remaining = 1)

allone.timefinished.cr<-new("CResource",time_total = 1 , time_remaining = 0 ,
                            evaluations_total = 1 , evaluations_remaining = 1 ,
                            iterations_total = 1 , iterations_remaining = 1)

allone.evaluationsfinished.cr<-new("CResource",time_total = 1 , time_remaining = 1 ,
                                   evaluations_total = 1 , evaluations_remaining = 0 ,
                                   iterations_total = 1 , iterations_remaining = 1)

allone.allfinished.cr<-new("CResource",time_total = 1 , time_remaining = 0 ,
                           evaluations_total = 1 , evaluations_remaining = 0 ,
                           iterations_total = 1 , iterations_remaining = 0)

evalonly.full.cr<-new("CResource",time_total = NULL , time_remaining = NULL ,
                      evaluations_total = 1000 , evaluations_remaining = 1000 ,
                      iterations_total = NULL , iterations_remaining = NULL)

evalonly.half.cr<-new("CResource",time_total = NULL , time_remaining = NULL ,
                      evaluations_total = 1000 , evaluations_remaining = 500 ,
                      iterations_total = NULL , iterations_remaining = NULL)

evalonly.empty.cr<-new("CResource",time_total = NULL , time_remaining = NULL ,
                      evaluations_total = 1000 , evaluations_remaining = 0 ,
                      iterations_total = NULL , iterations_remaining = NULL)



# Constructor testing -----------------------------------------------------

test_that(desc = "CResource class constructor function",
          code = {
            ## Test negative, non-integer values
            expect_error({
              cresource(time=-1)
            })
            expect_error({
              cresource(iterations = -1)
            })
            expect_error({
              cresource(evaluations = -1)
            })
            expect_error({
              cresource(iterations = 1.5)
            })
            expect_error({
              cresource(evaluations = 0.5)
            })
            ## Test correct instantiations
            expect_equal(cresource(),empty.cr)
            expect_equal(cresource(1,1,1),allone.cr)
          }
)

test_that(desc = "CResource class raw constructor",
          code = {
            ## Test setting one existing value and the other as NULL
            expect_error({
              new("CResource",time_remaining=NULL, time_total=10)
            })
            expect_error({
              new("CResource",iterations_remaining=NULL, evaluations_total=10)
            })
            expect_error({
              new("CResource",evaluations_remaining=NULL, evaluations_total=10)
            })
          }
)



# '-' function testing ----------------------------------------------------

test_that(desc = "CResource's '-' function",
          code = {
            ## Correct working
            res<-c(0,0,0)
            names(res)<-c("time","evaluations","iterations")
            expect_equal(allone.cr-allone.cr, res)
            res[1:3]<- -1
            expect_equal(allone.allfinished.cr-allone.cr,res)
            
            expect_true(all(is.na(allone.allfinished.cr-empty.cr)))
            expect_true(allone.allfinished.cr-allone.cr,res)
            expect_true(all(is.na(allone.allfinished.cr-empty.cr)))
            
          }
)



# decreaseResource function testing ---------------------------------------

test_that(desc = "CResource's decreaseResource function",
          code = {
            ## Errors
            expect_error({
              decreaseResources(empty.cr, NULL,1,1)
            })
            expect_error({
              decreaseResources(empty.cr, 1,NULL,1)
            })
            expect_error({
              decreaseResources(empty.cr, 1,1,NULL)
            })
            expect_error({
              decreaseResources(empty.cr, NA,1,1)
            })
            expect_error({
              decreaseResources(empty.cr, 1,NA,1)
            })
            expect_error({
              decreaseResources(empty.cr, 1,1,NA)
            })
            ## Correct working
            expect_equal(decreaseResources(empty.cr, 1,1,1), empty.cr)
            expect_equal(decreaseResources(allone.cr, 1,1,1), allone.allfinished.cr)
          }
)

# is.finished function testing ---------------------------------------

test_that(desc = "CResource's is.finished function",
          code = {
            ## Correct working
            expect_true(!is.finished(empty.cr))
            expect_true(!is.finished(evalonly.full.cr))
            expect_true(!is.finished(evalonly.half.cr))
            expect_true(is.finished(allone.allfinished.cr))
            expect_true(is.finished(allone.evaluationsfinished.cr))
            expect_true(is.finished(allone.timefinished.cr))
            expect_true(is.finished(evalonly.empty.cr))
          }
)

# what.finished function testing ---------------------------------------

test_that(desc = "CResource's what.finished function",
          code = {
            ## Correct working
            expect_equal(length(what.finished(empty.cr)),0)
            expect_equal(length(what.finished(allone.cr)),0)
            
            expect_true(all(c("time","evaluations","iterations") %in% what.finished(allone.allfinished.cr)))
            expect_true(!all(c("time","evaluations","iterations") %in% what.finished(allone.evaluationsfinished.cr)))
            expect_true(!all(c("time","evaluations","iterations") %in% what.finished(allone.timefinished.cr)))
            expect_true(all(c("evaluations") %in% what.finished(allone.evaluationsfinished.cr)))
            expect_true(all(c("time") %in% what.finished(allone.timefinished.cr)))
          }
)

# remaining.time function testing ---------------------------------------

test_that(desc = "CResource's remaining.time function",
          code = {
            ## Correct working
            expect_equal(remaining.time(allone.cr),1)
            expect_equal(remaining.time(allone.timefinished.cr),0)
            expect_equal(remaining.time(allone.evaluationsfinished.cr),1)
            expect_equal(remaining.time(allone.allfinished.cr),0)
            
            expect_true(is.null(remaining.time(empty.cr)))
            expect_true(is.null(remaining.time(evalonly.full.cr)))
            expect_true(is.null(remaining.time(evalonly.empty.cr)))
            expect_true(is.null(remaining.time(evalonly.half.cr)))
          }
)


# remaining.evaluations function testing ---------------------------------------

test_that(desc = "CResource's remaining.evaluations function",
          code = {
            ## Correct working
            expect_equal(remaining.evaluations(allone.cr),1)
            expect_equal(remaining.evaluations(allone.timefinished.cr),1)
            expect_equal(remaining.evaluations(allone.evaluationsfinished.cr),0)
            expect_equal(remaining.evaluations(allone.allfinished.cr),0)
            expect_equal(remaining.evaluations(evalonly.empty.cr),0)
            expect_equal(remaining.evaluations(evalonly.full.cr),1000)
            expect_equal(remaining.evaluations(evalonly.half.cr),500)
            
            expect_true(is.null(remaining.iterations(empty.cr)))
          }
)


# remaining.iterations function testing ---------------------------------------

test_that(desc = "CResource's remaining.iterations function",
          code = {
            ## Correct working
            expect_equal(remaining.iterations(allone.cr),1)
            expect_equal(remaining.iterations(allone.timefinished.cr),1)
            expect_equal(remaining.iterations(allone.evaluationsfinished.cr),1)
            expect_equal(remaining.iterations(allone.allfinished.cr),0)
            
            expect_true(is.null(remaining.iterations(empty.cr)))
            expect_true(is.null(remaining.iterations(evalonly.full.cr)))
            expect_true(is.null(remaining.iterations(evalonly.empty.cr)))
            expect_true(is.null(remaining.iterations(evalonly.half.cr)))
          }
)


## Delete existing objects
rm(list=ls())
gc()