/* Makes CLISP jump out of infinite loop */
address CLISP1

'(throw ''system::debug ''system::quit)'
