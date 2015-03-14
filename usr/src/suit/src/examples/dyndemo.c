/* 
 * 
 */ 
 
#include "suit.h" 

/* this is an example struct -- pretty big and ugly, but 
 * just imagine that you are going to use it and that you'd want a 
 * dynamic arary of these things.
 *  
 */

typedef struct foo { 
	double  realnum1; 
	double  realnum2; 
	char   string[50]; 
	double  realnum3; 
	int    myinteger; 
	char   anotherstring [30]; 
	}my_ugly_struct; 

int compareStructs(void *first, void *second) 
{
    my_ugly_struct *struct1 = (my_ugly_struct *)first;
    my_ugly_struct *struct2 = (my_ugly_struct *)second;

    return((struct1->myinteger) -  (struct2->myinteger));
}

/*********************************/
/****  GETTING ELEMENTS OUT  *****/
/*********************************/
/* 
 * Here is an idiom that you will see all the time when dealing 
 * with DynArrays:  
 *       foo = (fooType *) DynGet(ArrayOfFooTypes, index);
 * 
 * DynGet returns a (void *) which must be cast to a pointer of 
 * the correct type in order to get at a pointer to the data.
 * NOTE THAT YOU ARE NOW HANDLING A POINTER DIRECTLY INTO THE
 * DYNARRAY, AND NOT A COPY OF THE DATA.
 * This also shows the usual way of looping through the elements of 
 * a DynArray. This is a utility Function used in this demo.
 */

void PrintDynArray(DynArray obj) {
    my_ugly_struct *data;
    int i, slot;

    for (i=DynLow(obj); i <= DynHigh(obj); i++) {
	data = (my_ugly_struct *) DynGet(obj, i);
	printf("Element %d is %d.\n", i, (int) data->myinteger); 
    }
    printf("\n\n");
}
 
void main(int argc, char **argv) 
{ 
     DynArray	obj;
     int	i, slot; 
     my_ugly_struct d;
 

     /********************/
     /****  CREATING *****/
     /********************/
     /* 
      * Here, we create a Dynarray of "my_ugly_struct"s 
      * and we supply two things:
      *       1.) The size required for each entry 
      *           using the sizeof() operator in C on the
      *           datatype "my_ugly_struct"
      *       2.) The number of elements to add to the 
      *           DynArray each time it needs to be expanded (when 
      *           there is a request to add another element and all 
      *           the slots are taken.
      *           
      *           For our example, we ask for 1 more slot, but in 
      *           general, this number is dependent on your application.
      *           Typically a speed/space tradeoff -- don't want 
      *           to ask for slots you might not use -- don't 
      *           want to keep coming back for new elements all the time.
      */
     
     obj = DynCreate(sizeof(my_ugly_struct), 1); 

     /*********************************/
     /****  PUTTING ELEMENTS IN   *****/
     /*********************************/
     /* 
      * Notice that each time through the loop, we assign an 
      * integer to the field of the struct. The DynArray will 
      * COPY THE DATA THAT YOU HAND IT, so this system of having
      * a "dummy" object like "d" here works just fine. This is 
      * done so that you can add elements to a DynArray that go out of 
      * scope, but still persist in the DynArray.
      * 
      * Notice that the call to DynAdd carries the name of the 
      * DynArray and the ADDRESS of the data being added. 
      */
     
     for (i=0; i<10; i++) { 
	 d.myinteger = i; 
	 DynAdd(obj, &d);
     } 
     
     /*********************************************/
     /****  PRINTING ELEMENTS OF A DYNARRAY *******/
     /*********************************************/
     /* Here we use a utility function cooked up for
      *	this demo. It appears at the top of this file.
      */
     printf("The elements of the array after adding them:\n");
     PrintDynArray(obj);


     /*********************************************/
     /****  DELETING ELEMENTS FROM A DYNARRAY *****/
     /*********************************************/
     /* Here we delete several items from the middle of the
      * array. In general, of course, you should be careful that 
      * the elements actually exist before you delete them. 
      * NOTICE THAT THE INDICES OF THE ELEMENTS CHANGE
      * AFTER THE DELETE HAPPENS.
      */
     DynDelete(obj,1);
     DynDelete(obj,3);
     printf("The elements of the array after deleting a few\n");
     PrintDynArray(obj);


     /***************************************************/
     /*****  SEARCHING FOR ELEMENTS OF A DYNARRAY *******/
     /***************************************************/
     /* Here we look for an Element of the DynArray. 

     For purposes of this demo, we say two structs "match" if
     the "myinteger" fields are the same. We write a compare
     function that tests for this (the compare routine takes two
     (void *)'s and returns the same values as strcmp() 

     Now we hand the DynFindIndex() Function the dynarray, a pointer to      
     a representative element that we want to compare against 
     (you're saying : "look for an element that looks like this") 
     and, of course, the comparison function.
      */
     d.myinteger = 2;

     slot = DynFindIndex(obj, &d, compareStructs);
     printf ("The matching struct was found in slot %d\n", slot);


     /*********************************/
     /****  DESTROYING A DYNARRAY *****/
     /*********************************/
     /* Memory held by the DynArray is freed when the DynArray is destroyed.
      * Further operations on the DynArray (DynAdd, DynDelete, etc.) 
      * are undefined and will cause fire and pestilence if you call them.
      * Of course, you can always call DynCreate again on obj, in which
      * case, everything works fine.
      */
     DynDestroy(obj); 
 
     printf ("Done\n"); 
} 
