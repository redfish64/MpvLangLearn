from gooey import Gooey, GooeyParser
from message import display_message
import os
import os.path
import sys

nonbuffered_stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)
sys.stdout = nonbuffered_stdout
  
@Gooey(program_name="MpvLangLearn",required_cols=1,optional_cols=1,advanced=True)
def main():
  desc = "\nLearn a foreign language by playing movies"
  srtNone = "<none>"

  p = GooeyParser(description=desc)
  sp = p.add_subparsers(help='commands', dest='command')

  simple = sp.add_parser('simple', help='Simple options for running program')

  simple.add_argument("Video", help="Video to play", widget="FileChooser")
  simple.add_argument("NativeSrt", help="Native Language Subtitle (.srt) file", widget="FileChooser")
  simple.add_argument("-fs","--ForeignSrt", help="Foreign Language Subtitle (.srt) file. (Will be displayed before Native srt file)", widget="FileChooser")
  
  advanced = sp.add_parser('advanced', help='Advanced options for running program')
  advanced.add_argument("Video", help="Video to play", widget="FileChooser",default="a")
  advanced.add_argument("EnableEmptyLoop", action="store_true", help="Enable a loop with no subtitles. Otherwise subtitles will always be displayed (either first or second)")
  advanced.add_argument('EmptyTrackSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='If Empty Loop enabled, speed of video while no subtitle is displayed (only while people are talking)')
  advanced.add_argument("Loop1Srt", help="These subtitles will be displayed first", widget="FileChooser",default="b")
  advanced.add_argument('FirstSrtSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='Speed of video while showing subtitles from first subtitle file')
  advanced.add_argument('-l2s',"--Loop2Srt", help="Second Subtitle file",widget="FileChooser")
  advanced.add_argument('-sss','--SecondSrtSpeed', choices=['1.5','1.25','1.0','0.9','0.75'], default='1.0', help='Speed of video while showing subtitles from second subtitle file')
  advanced.add_argument('-ls','--LeadSeconds', type=float, default=1.0, help='Seconds before each subtitle starts to start loop')
  advanced.add_argument('-ts','--TailSeconds', type=float, default=1.0, help='Seconds after each subtitle ends to wait to end loop')

  args = p.parse_args()
  currdir = os.path.dirname(os.path.realpath(__file__))

  # if args.command == "simple":
  #   args.FirstTrackSpeed='1.0'
  #   args.FirstTrackSrtId='none'
  #   args.LeadSeconds=1.0
  #   args.SecondTrackSpeed='1.0'
  #   args.SecondTrackSrtId='1'
  #   args.TailSeconds=1.0
  #   if(args.SrtFile2 != srtNone && isfile args.SrtFile2):
  #     args.ThirdTrackSpeed='1.0'
  #     args.ThirdTrackSrtId='2'
  #   else:
  #     args.ThirdTrackSrtId='<disabled>'
  # else:
  #   args.FirstTrackSpeed='1.0'
  #   args.FirstTrackSrtId='none'
  #   args.LeadSeconds=1.0
  #   args.SecondTrackSpeed='1.0'
  #   args.SecondTrackSrtId='1'
  #   args.TailSeconds=1.0
  #   if(args.SrtFile2 == srtNone || !(isfile args.SrtFile2)):
  #     args.ThirdTrackSrtId='<disabled>'

      
  print args
  # os.execv(currdir+'/fakeout',args)

def here_is_smore():
  pass


if __name__ == '__main__':
  main()
