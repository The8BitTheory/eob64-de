import java.io.*;
import java.util.*;

public class ExtractScript {
	public static String stringPool = "";

	private static String fixCase(String str) {
		String out="";
		boolean shouldUpperCase = true;
		for (int i=0; i<str.length(); i++) {
			char ch = str.charAt(i);
			if (ch == '.')
				shouldUpperCase = true;
			if (shouldUpperCase && Character.isLetter(ch)) {
				out += Character.toUpperCase(ch);
				shouldUpperCase = false;
				continue;
			}
			out += ch;
		}
		return out;
	}

	public static void extract(PrintStream out, int levelIndex, Inf inf) throws IOException {
		HashMap<Integer, String> address2Label = new HashMap<Integer, String>();
		HashMap<Integer, Trigger> address2Trigger = new HashMap<Integer, Trigger>();

		AsmUtil.dumpName(out, String.format("lvl%dTriggers", levelIndex));
		int i=0;
		for (Trigger trigger : inf.script.triggers) {
			int address = trigger.address - inf.scriptOffset;
			String label = String.format("_%d_%d", levelIndex,address);
			i++;
			address2Label.put(address, label);
			address2Trigger.put(address, trigger);
			out.printf("\t.byte <$%04x, >$%04x, $%02x, <%s, >%s ;[%d,%d]\n", trigger.rawPos, trigger.rawPos, trigger.flags, label, label, trigger.pos.x, trigger.pos.y);
		}
		out.printf("\t.byte $ff,$ff\n");

		// Emit all the script data
		Trigger refTrigger=null;
		int lastRefX = -1;
		int lastRefY = -1;
		for (AbstractToken token : inf.script.tokens) {		
			Trigger trigger = address2Trigger.get(token.offset);
			if (trigger!=null) {
				refTrigger = trigger;	
				lastRefX = refTrigger.pos.x;
				lastRefY = refTrigger.pos.y;		
				out.printf("; Referenced by trigger! Pos:[%d,%d] Flags:%%", refTrigger.pos.x, refTrigger.pos.y);
				for (int b=7;b>=0;b--) {
					int v = ((1<<b)&refTrigger.flags)==0?0:1;
					out.printf("%d",v);
				}
				out.println();
			}
			out.printf("; %s\n", token.getClass().getName());

			AsmUtil.dumpLocalName(out,String.format("_%d_%d", levelIndex,token.offset));

			// Optimize MessageTokens using a StringPool
			if (token instanceof MessageToken) {
				int vgaColors[]={0,6,5,3,2,4,9,0xf,0xb,0xe,0xd,3,0xa,4,7,1};
	
				MessageToken mt = (MessageToken)token;
				String message = fixCase(mt.message);
				int messageOffset = stringPool.indexOf(message);
				if (messageOffset==-1) {
					messageOffset = stringPool.length();
					stringPool += message;
				}
				out.printf(".byte $%02x, $%02x, <(messagePool+$%04x), >(messagePool+$%04x), $%02x ;\"%s\"\n", AbstractToken.TOKEN_MESSAGE, message.length(), messageOffset, messageOffset, vgaColors[mt.color], message);
			}

			// Resolve address of JumpTokens
			else if (token instanceof JumpToken) {
				JumpToken jt = (JumpToken)token;
				out.printf("\t.byte $%02x, <_%d_%d, >_%d_%d\n", AbstractToken.TOKEN_JUMP, levelIndex, jt.address- inf.scriptOffset, levelIndex, jt.address- inf.scriptOffset);
			}

			// Resolve address of CallToken
			else if (token instanceof CallToken) {
				CallToken jt = (CallToken)token;
				out.printf("\t.byte $%02x, <_%d_%d, >_%d_%d\n", AbstractToken.TOKEN_CALL, levelIndex, jt.address- inf.scriptOffset, levelIndex, jt.address- inf.scriptOffset);
			}

			// Resolve address of ConditionalTokens
			else if (token instanceof ConditionalToken) {
				ConditionalToken ct = (ConditionalToken)token;
				byte[] data = new byte[ct.rawData.length-2];
				System.arraycopy(ct.rawData, 0, data, 0, data.length);
				AsmUtil.dumpByteArray(out, null, data);
				out.printf("\t.word _%d_%d\n", levelIndex, ct.falseAddress - inf.scriptOffset);
			}

			// Highlight encounters
			else if (token instanceof EncounterToken) {
				EncounterToken et = (EncounterToken)token;
				out.printf("\t;Encounter $%02x at %d,%d on level %d\n", et.index, lastRefX, lastRefY, levelIndex);
		 		AsmUtil.dumpByteArray(out, null, token.rawData);				
			}
			else
		 		AsmUtil.dumpByteArray(out, null, token.rawData);
	 	}
	}
}