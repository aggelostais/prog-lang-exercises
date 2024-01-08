#include <stdio.h>
#include <stdlib.h>

/*returns binary of t*/
void tobin(long t, int * res)
{
	
	int i = 0;
	while(t > 0 && i < 32)
        {
                res[i] = t % 2;
                t = t / 2;
                i++;
        }

}

int count_ones(int * table, int size)
{
	int res = 0;
	for(int i = 0; i < size; i++)
		if(table[i] == 1) res++;

	return res;
}

int breakin(int * list, long k)
{
   for(long times = k; times > -1; times--)
      { 
	if(times < 0) printf("invalid call (breakin)\n"); 

	if(times == 0); // end of recursion
        else
        {
		int i = 1;
		int found = 0; //bool flag
		while(i < 32 && !found)
		{

			if(list[i] > 0)
			{
				list[i] = list[i] - 1;
				list[i-1] = list[i-1] + 2;
				found = 1;
			}
			i++;
		}
        }

      }

}

void print_list(int * list, int size)
{
	size--;
	while(list[size] == 0) size--;

	printf("[");
	
	for(int i = 0; i <= size; i++)
	{
		if(i < size) printf("%d,", list[i]);
		else printf("%d", list[i]);
	}

	printf("]\n");
	
}


/*powers will return 1 if n can be written in exactly k powers of 2, and will return 0 if that is not possible
 * */
int powers(unsigned long  n, unsigned long k)
{
	if(k <= 0)
	{
		printf("cannot run powers with k <= 0\n");
		return -1;
	}
	if(k > n)
	{
		printf("[]\n");
		return 0;
	}

	int binary_digits[32];
	for(int i = 0; i < 32; i++) binary_digits[i] = 0;

	int copy[32];
	
	tobin(n, binary_digits);

	long ones = count_ones(binary_digits, 32);

	if(k < ones)
	{
		printf("[]\n");
                return 0;
        }

	//copy
	for(int i = 0; i < 32; i++)
		copy[i] = binary_digits[i];

	breakin(copy, k - ones);
	print_list(copy, 32);
	
	return 1;

}


int main(int argc, char ** argv)
{
	if( argc != 2 )
	{
		printf("please give one input file\n");
		return 0;
	}
	
	FILE * file;
	file = fopen(argv[1], "r");
	
	if(file == NULL)
	{
		printf("error! opening the file failed\n");
		exit(1);
	}

	int input_size = 0;
       	fscanf(file, "%d", &input_size);
	

	for(int i = 0; i < input_size; i++)
	{
		long n, k;

		fscanf(file, "%ld", &n);
		fscanf(file, "%ld", &k);
		
		powers(n, k);
	}

	fclose(file);

	return 0;
}
