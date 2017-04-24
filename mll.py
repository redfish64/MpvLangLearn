from gooey import Gooey, GooeyParser
from message import display_message
import os
import os.path
import sys

nonbuffered_stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)
sys.stdout = nonbuffered_stdout
  
@Gooey(program_name="MpvLangLearn",required_cols=1,optional_cols=2,advanced=True)
def main():
  desc = "\nLearn a foreign language by playing movies"
  srtNone = "<none>"

  p = GooeyParser(description=desc)
  sp = p.add_subparsers(help='commands', dest='command')

  simple = sp.add_parser('simple', help='Simple options for running program')

  simple.add_argument("Video", help="Video to play", widget="FileChooser",default="a")
  simple.add_argument("Srt1", help="First Subtitle file", widget="FileChooser",default="b")
  simple.add_argument("Srt2", help="Second Subtitle file",default=srtNone, widget="FileChooser")
  
  advanced = sp.add_parser('advanced', help='Advanced options for running program')

  advanced.add_argument("Video", help="Video to play", widget="FileChooser",default="a")
  advanced.add_argument("SrtFile1", help="First Subtitle file", widget="FileChooser",default="b")
  advanced.add_argument("SrtFile2", help="Second Subtitle file",default="<none>", widget="FileChooser")
  advanced.add_argument('-t1sid','--FirstTrackSrtId', choices=['none','1','2','<disabled>'], default='none', help='Srt for first track. "none" means not display subtitles')
  advanced.add_argument('-t1sp','--FirstTrackSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='First Track Speed')
  advanced.add_argument('-t2sid','--SecondTrackSrtId', choices=['none','1','2','<disabled>'], default='1', help='Srt for second track.')
  advanced.add_argument('-t2sp','--SecondTrackSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='Second Track Speed')

  advanced.add_argument('-t3id', '--ThirdTrackSrtId', choices=['none','1','2','<disabled>'], default='2', help='Srt for third track. (optional)')
  #group = advanced.add_argument_group('group') #does nothing
  advanced.add_argument('-t3sp', '--ThirdTrackSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='Third Track Speed (optional)')
  advanced.add_argument('-ls', '--LeadSeconds', type=float, default=1.0, help='Seconds before subtitle starts to start loop')
  advanced.add_argument('-ts', '--TailSeconds', type=float, default=1.0, help='Seconds after subtitle ends to end loop')

  args = p.parse_args()
  currdir = os.path.dirname(os.path.realpath(__file__))

  if args.command == "simple":
    args.FirstTrackSpeed='1.0'
    args.FirstTrackSrtId='none'
    args.LeadSeconds=1.0
    args.SecondTrackSpeed='1.0'
    args.SecondTrackSrtId='1'
    args.TailSeconds=1.0
    if(args.SrtFile2 != srtNone && isfile args.SrtFile2):
      args.ThirdTrackSpeed='1.0'
      args.ThirdTrackSrtId='2'
    else:
      args.ThirdTrackSrtId='<disabled>'
  else:
    args.FirstTrackSpeed='1.0'
    args.FirstTrackSrtId='none'
    args.LeadSeconds=1.0
    args.SecondTrackSpeed='1.0'
    args.SecondTrackSrtId='1'
    args.TailSeconds=1.0
    if(args.SrtFile2 == srtNone || !(isfile args.SrtFile2)):
      args.ThirdTrackSrtId='<disabled>'

      
  print args
  os.execv(currdir+'/fakeout',args)

def here_is_smore():
  pass


if __name__ == '__main__':
  main()
