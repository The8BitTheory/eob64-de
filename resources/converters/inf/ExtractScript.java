import java.io.*;
import java.util.*;

public class ExtractScript {
	public static String stringPool = "";
	public static Map<String, String> translated = new HashMap<>();
	
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
				String m2 = translated.get(message);
				if (m2 != null) {
					message = m2;
				}
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
	
	static {
		translated.put("Going down...","Gehe runter...");
		translated.put("You can't go that way.","Hier geht's nicht weiter.");
		translated.put("It smells terrible here.","Es riecht furchtbar hier.");
		translated.put("This slimy, smelly drain pipe reveals nothing.","Dieses schleimige, stinkende Abflussrohr lässt nichts erkennen.");
		translated.put("There is a drainage grate here.","Hier ist ein Abflussrost.");
		translated.put("Something scurries deeper into the floor drain.","Etwas huscht tiefer in den Abfluss im Boden.");
		translated.put("Kobold rune for 'entrance'.","Kobold-Rune für 'Eingang'.");
		translated.put("This rune is unrecognized.","Diese Rune ist unbekannt.");
		translated.put("Special qüst for this level!","Extraaufgabe auf dieser Ebene!");
		translated.put("Fallen rocks block the way.","Heruntergefallene Felsen blockieren den Weg.");
		translated.put("It dösn't fit.","Das passt nicht.");
		translated.put("Failed lock pick attempt.","Das Schloss lässt sich nicht knacken.");
		translated.put("The lock picks break!.","Der Dietrich bricht!");
		translated.put("Only a thief can pick locks.","Nur Diebe können Schlösser knacken.");
		translated.put("This lock requires a key.","Für dieses Schloss braucht man einen Schlüssel.");
		translated.put("The lock appears jammed.","Das Schloss klemmt wohl.");
		translated.put("Going up...","Gehe hoch...");
		translated.put("The lock has been picked!","Das Schloss ist geknackt!");
		translated.put("Dead end?","Sackgasse?");
		translated.put("The dagger fits.","Der Dolch passt.");
		translated.put("What an odd carving to place here.","Was für eine seltsame Rune das ist.");
		translated.put("A hollow laughter echös faintly.","Dumpfes Gelächter verhallt in der Ferne.");
		translated.put("A whispering voice says 'illusion, solid illusion'","Eine Stimme flüstert: 'Illusion, solide Illusion'");
		translated.put("The door appears stuck.","Die tür scheint zu klemmen.");
		translated.put("There seems to be movement inside this floor drain.","In dem Abfluss im Boden scheint sich etwas zu bewegen.");
		translated.put("Entry level.","Eingangsebene.");
		translated.put("Death section.","Todessektion.");
		translated.put("Correction facility.","Umerziehungseinrichtung.");
		translated.put("The room seems to move.","Der Raum scheint sich zu bewegen.");
		translated.put("This carving looks familiar.","Diese Rune sieht bekannt aus.");
		translated.put("This is an orc rune of passage.","Das ist eine orkische Passierrune.");
		translated.put("This looks like a 'travel' marker.","Das sieht wie eine Wegmarkierung aus.");
		translated.put("Not all is as it appears","Nicht alles ist wie es erscheint.");
		translated.put("Watch your step.","Geht vorsichtig.");
		translated.put("Only the strong shall pass.","Nur die Starken sollen passieren");
		translated.put("Chamber of the great warriors.","Kammer der großen Krieger.");
		translated.put("You feel dizzy.","Ihr fühlt euch schwindelig.");
		translated.put("The key fits.","Der Schlüssel passt.");
		translated.put("Stay thy hand at the first opportunity, for the second will yield greater reward.","Lass es bleiben bei der ersten Gelegenheit, so wird die Zweite höher belohnt.");
		translated.put("This hole looks like the inside of a jewel box.","Dieses Loch sieht wie die Innenseite einer Schmuckschatulle aus.");
		translated.put("This hole is tightly sealed.","Dieses Loch ist fest versiegelt.");
		translated.put("Museum.","Museum.");
		translated.put("Riches are a small sacrifice for greater freedom.","Kostbarkeiten sind ein kleines Opfer für mehr Freiheit.");
		translated.put("What gös around, comes around.","Wie man in den Wald ruft, so schallt es heraus.");
		translated.put("City of Waterdeep - rapid access transport system","RATS - Schnell");
		translated.put("Doors open, doors close.  Items come, items go.","Türen öffnen, Türen schliessen.   Habe kommt, Habe geht.");
		translated.put("Dwarvish writing of some kind.","Irgendetwas in Zwergenschrift.");
		translated.put("Access control lever.","Zugangskontrollschalter.");
		translated.put("Emergency exit.","Notausgang.");
		translated.put("Dwarvish totem of treasure guarding.","Zwergentotems als Schatzwache.");
		translated.put("This is a dwarven kingdom marker.","Dies ist die Grenzmarke eines Zwergenkönigreiches.");
		translated.put("Feeding instructions of some kind.","Irgendwelche Fütterungsanweisungen.");
		translated.put("Krün - king under the mountain.","Krün - König unter dem Berg.");
		translated.put("Krün - the holder of wisdom.","Krün - Träger der Weisheit.");
		translated.put("Krün - destroyer of the ancient one.","Krün - Zerstörer des Alten Volkes.");
		translated.put("Krün - the fearless one.","Krün - der Furchtlose.");
		translated.put("There is evil that lies beyond this room.","Das Böse lauert hinter diesem Raum.");
		translated.put("Oracle of knowledge.","Orakel des Wissens.");
		translated.put("You've made it this far. Good luck.","Bis hierher habt ihr es geschafft. Viel Glück.");
		translated.put("Things are not always as they appear","Die Dinge sind nicht immer wie sie scheinen.");
		translated.put("Please reset drain holes when finished.","Zum Schluss bitte die Abflusslöcher zurücksetzen.");
		translated.put("Greed will be your downfall.","Gier wird euer Untergang sein.");
		translated.put("Dwarvish rune of 'safe passage'.","Zwergenrune für 'Sicherer Durchgang'.");
		translated.put("This is a dwarvish rune.","Dies ist eine Zwergenrune.");
		translated.put("Pantry","Speisekammer");
		translated.put("Looks like the dwarvish word for 'closet' or 'cupboard'.","Sieht wie das Zwergenwort für 'Schrank' oder 'Kabinett' aus.");
		translated.put("Store weapons before proceeding.","Vor dem Weitergehen, Waffen ablegen.");
		translated.put("Dwarvish writing saying 'fasten' and 'hammer'.","Zwergenschrift, die besagt 'Hammer' und 'Festschnallen'.");
		translated.put("Veritcal access shaft.","Vertikaler Zugangsschacht.");
		translated.put("Silverware rack... Be neat.","Besteckregal... seid ordentlich.");
		translated.put("Dart rack.","Wurfpfeilständer.");
		translated.put("Key taken.","Schlüssel genommen.");
		translated.put("Round and round...","Immer rundherum...");
		translated.put("Nest","Nest");
		translated.put("This is the symbol of a dwarven tribe.","Das ist das Symbol eines Zwergenstammes.");
		translated.put("A firing mechanism of some sort.","Eine Art Abschussmechanismus.");
		translated.put("Nothing can be seen within the holes.","In den Löchern ist nichts zu sehen.");
		translated.put("Don't delay!","Nicht trödeln!");
		translated.put("It looks like these holes are scorched.","Es sieht aus, als wären diese Löcher versengt.");
		translated.put("Test of courage.","Mutprobe.");
		translated.put("Treasure room.  No admittance.","Schatzkammer.  Kein Zutritt.");
		translated.put("One sacrifice made...","Ein geleistetes Opfer...");
		translated.put("Thank you for your donation.","Vielen Dank für ihre Spende.");
		translated.put("One leap of faith...","Ein Schritt des Glaubens...");
		translated.put("One battle for glory...","Eine Schlacht für die Ehre...");
		translated.put("Magic","Magie");
		translated.put("Armor","Rüstung");
		translated.put("Weapon","Waffe");
		translated.put("No turning back.","Kein zurück.");
		translated.put("One way.","Einbahn.");
		translated.put("Fight for your freedom.","Kämpft für eure Freiheit.");
		translated.put("Use front entrance.  Exit only.","Vorderen Eingang benutzen. Nur Ausgang.");
		translated.put("No trespassing.","Kein Durchgang.");
		translated.put("These holes look scorched.","Diese Löcher sehen versengt aus.");
		translated.put("One gem for one key","Ein edelstein pro Schlüssel");
		translated.put("One's faith repaid.","Jemandes ausbezahlter Glaube.");
		translated.put("Turn around.","Dreht um.");
		translated.put("You shouldn't have pressed the button.","Ihr hättet den Knopf nicht drücken sollen.");
		translated.put("Don't press the button.","Nicht den Knopf drücken.");
		translated.put("Only one choice allowed.","Nur eine Wahl erlaubt.");
		translated.put("Gaultlet of flame. Turn back!","Flammender Panzerhandschuh. Dreht um!");
		translated.put("Looks like a mounting device of some sort.","Sieht wie eine Art Steighilfe aus.");
		translated.put("Storage","Lager");
		translated.put("A glowing rock is firmly mounted into the wall.","Ein glühender Stein ist fest in der Wand verankert.");
		translated.put("The rock fits!","Der Stein passt!");
		translated.put("You need a special key.","Ihr benötigt einen speziellen Schlüssel.");
		translated.put("Watch your head...","Passt auf eure Köpfe auf...");
		translated.put("Hall of thieves.","Halle der Diebe.");
		translated.put("You forgot something.","Ihr habt etwas vergessen.");
		translated.put("Thank you.","Danke schön.");
		translated.put("The cunning and agile shall survive.","Die Listigen und Agilen sollen überleben.");
		translated.put("Combination lock -- be quick.","Kombinationsschloss -- Beeilung.");
		translated.put("Drow word for 'button'.","Dunkelelfenwort für 'Knopf'.");
		translated.put("Be generous.","Seid großzügig.");
		translated.put("Drow writing of some kind.","Irgendwelche Dunkelelfenworte.");
		translated.put("Donate sword","Spendet ein Schwert");
		translated.put("Donate food","Spendet Essen");
		translated.put("Donate armor","Spendet Rüstung");
		translated.put("Donate missile","Spendet ein Geschoss");
		translated.put("Oracle of devouring","Orakel der Eifersucht");
		translated.put("It is written, the key lies on the other side.","Es steht geschrieben, der Schlüssel liegt auf der anderen Seite.");
		translated.put("Drow writing is written here.","Dunkelelfenworte stehen hier.");
		translated.put("Stow yer weapons.","Verstaut eure Waffen.");
		translated.put("Do not disturb the hives.","Stört den Stock nicht.");
		translated.put("Hive","Stock");
		translated.put("Proper seqünce","Korrekte Reihenfolge.");
		translated.put("Thirteen","Dreizehn");
		translated.put("Look behind you!","Schaut hinter euch!");
		translated.put("Keep an eye out for trouble.","Seid auf der Hut vor Ärger.");
		translated.put("Welcome","Willkommen");
		translated.put("In case of fire...","Im Falle eines Feuers...");
		translated.put("In case of flood...","Im Falle einer Überflutung...");
		translated.put("Jump...","Springt...");
		translated.put("This is a celestial star of navigation.","Dies ist ein himmlischer Stern zur Navigation.");
		translated.put("Your fate lies in the stars.","Euer Schicksal liegt in den Sternen.");
		translated.put("Leave no stone unturned.","Dreht jeden Stein um.");
		translated.put("Alignment must be trü.","Eure Einstellung muss stimmen.");
		translated.put("Chwat'","Chwat'");
		translated.put("Room of the key.","Raum des Schlüssels.");
		translated.put("This hole reeks faintly of smoke.","Dies Loch riecht leicht nach Rauch.");
		translated.put("Turn back, no tresspassing.","Dreht um! Kein Durchgang!");
		translated.put("You were warned.","Ihr wurdet gewarnt.");
		translated.put("You get spiked!","Ihr werdet durch Spieße verletzt!");
		translated.put("Light beam has been broken.","Eine Lichtschranke wurde unterbrochen.");
		translated.put("Room of the spheres.","Raum der Kugeln.");
		translated.put("Stone for substance.","Stein für Substanz.");
		translated.put("Potion for strength.","Trank für Stärke.");
		translated.put("Sphere for animation.","Kugel für Bewegung.");
	}
}
