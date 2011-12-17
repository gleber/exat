import jade.core.Agent;
import jade.core.AID;
import jade.lang.acl.ACLMessage;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.MessageTemplate;

public class JADEAgent extends Agent {

private MessageTemplate template = 
   MessageTemplate.MatchPerformative(ACLMessage.REQUEST);

protected void setup() {
   System.out.println("[Agent: "+this.getLocalName() +
         "] Starting");

   addBehaviour(new CyclicBehaviour(this) {
      public void action() {
         ACLMessage msg = myAgent.receive(template);
         if (msg != null) {
            System.out.println("[Agent: " +
               myAgent.getLocalName() +
               "] Request received from agent " +
               msg.getSender().getName());
         }
         else {
            block();
         }
      }
   } );

   sendMessage();
}

private void sendMessage() {
   AID r = new AID ("the_exat_agent@exatplatform", AID.ISGUID);
   r.addAddresses("http://localhost:7779/acc");
   ACLMessage aclMessage = new ACLMessage(ACLMessage.REQUEST);
   aclMessage.addReceiver(r);
   aclMessage.setContent("ping");
   this.send(aclMessage);
}
}

