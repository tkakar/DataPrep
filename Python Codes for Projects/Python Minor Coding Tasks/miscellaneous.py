####### print frizzbuzz question
for i in range(1,101):
    if i%3==0 and i%5==0:
        print (i, "frizzBuzz")

    elif i%3==0:
        print (i, "Buzz")

    elif i%5==0:
        print (i, "frizz")

    else: print (i)

### return indices of two numbers whose sum add to a target number
nums = [1, 2, 7, 11, 15, 19];
target = 26;
result=[];
for i in range(0, len(nums)):
    for j in range(i+1, len(nums)):
        if nums[i] + nums[j] == target:
            break


# when array is sorted ####
my_map = {};
size = len(nums);
for i in range( size):
    my_map[nums[i]] = i;

print my_map, target;
for i in range(size):
    if target - nums[i] in my_map:
        print [i+1, my_map[target - nums[i]]+1]
        break;


######## Code to check if sum of two consecutive elements in array is equal to target
numbers = [2,7,11,15]
target = 9;
# print target
for i in range(0, len(numbers)):
    for j in range(i+1, len(numbers)):
        # print (numbers[i] + numbers[j] == target)
        if  i < j and numbers[i] + numbers[j] == target:
            print "ans", [i+1, j+1]
                # else:
                #     continue;


# # optimize above code using hashing to remove the second loop

######## Code to reverse a number
def reverse(x):
        if x > 0:  # handle positive numbers
            a =  int(str(x)[::-1])
        if x <=0:  # handle negative numbers
            a = -1 * int(str(x*-1)[::-1])
        # handle 32 bit overflow
        mina = -2**31
        maxa = 2**31 - 1
        if a not in range(mina, maxa):
            return 0
        else:
            return a

x = '123'
print reverse(x)
# print x[::-1]
