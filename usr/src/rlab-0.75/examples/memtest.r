//
// Beginning of test file `test.r' (just `load(test.r)' to test the
// memory problem)
//
load("estimate.r");
B = rand(10,10)/20+eye(10,10);
x = [1:10]';
f = B*x;
ans = 0;
h = [0;0;0;0;0;0;1;0;0;0];
for (i in 1:10) {
  ans[i] = 0;
  for (j in 1:5) {
    ans[i] = ans[i] + estimate(B, f, h, 10, 2^i);
    system("/usr/ucb/ps -aux | grep rlab");
  }
  ans[i] = ans[i] / 5
}
write("stdout",ans);
