#!/bin/perl

$ENV{'PATH'} =  "/bin:/usr/bin";

sub FileList {
  local ($dir)=@_;
  local (@files);
  opendir(DIR,$dir) || die "Coudln't open dir '$dir': $!";
  @files = grep(!/^\.$|^\..$/,readdir DIR);
  close DIR;
  @files;

}

foreach $file (@ARGV)
{
  ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$\
blocks) = stat($file);
  $file{$dev,$ino}="$file:  ";
#  write;
}
#print "\n";

format =
@<<<<<<<<<< @>>>>>>> @>>>>>>> @>>>>>>> @>>>>>>>>
$name, $dev, $ino, sprintf("0x%x",$rdev), $size
