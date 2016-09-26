module Types where

type EventType = String
type StreamType = String
type Name = String
type Value = String
type Source = String
type Timestamp = Integer

type Event = (EventType, Timestamp, [Attribute])
type Attribute = (Name, Value)
type Stream = (StreamType, Source, [Event])
type Store = [Stream]

updateStore :: ([Stream] -> [Stream]) -> Store -> Store
updateStore c s = c s

type CreateStreams = [Stream] -> [Stream]
type DeleteStreams = [Stream] -> [Stream]
type MergeStreams = [Stream] -> [Stream] 
type SplitStreams = [Stream] -> [Stream] 


updateStream :: UpdateStreamPredicate -> ([Event] -> [Event]) -> Stream -> Stream
updateStream p b s@(streamType, source, events) | p s       = (streamType, source, b events)
                                                | otherwise = s

type UpdateStreamPredicate = Stream -> Bool

type CreateEvents = [Event] -> [Event]
type DeleteEvents = [Event] -> [Event]
type MergeEvents = [Event] -> [Event]   
type SplitEvents = [Event] -> [Event]   

updateEventType :: (EventType -> EventType) -> Event -> Event
updateEventType b (eventType, timeStamp, attrs) = (b eventType, timeStamp, attrs)

updateEvent :: UpdateEventPredicate -> ([Attribute] -> [Attribute]) -> Event -> Event
updateEvent p b e@(eventType, timeStamp, attrs) | p e       = (eventType, timeStamp, b attrs)
                                                | otherwise = e

type UpdateEventPredicate = Event -> Bool

type CreateAttributes = [Attribute] -> [Attribute]
type DeleteAttributes = [Attribute] -> [Attribute]
type MergeAttributes = [Attribute] -> [Attribute]  
type SplitAttributes = [Attribute] -> [Attribute]  

type UpdateAttribute = Attribute -> Attribute    

-- Wrap the UpdateAttribute so that the resulting partially-applied function mathces what updateEvent expects
wrapUpdateAttribute :: UpdateAttributePredicate -> UpdateAttribute -> [Attribute] -> [Attribute]
wrapUpdateAttribute p f = map f'
    where f' x | p x = f x | otherwise = x
    
type UpdateAttributePredicate = Attribute -> Bool

type MergeStreamsNonSet = [Stream] -> Stream
type SplitStreamsNonSet = Stream -> [Stream]

type MergeEventsNonSet = [Event] -> Event
type SplitEventsNonSet = Event -> [Event]

type MergeAttributesNonSet = [Attribute] -> Attribute
type SplitAttributesNonSet = Attribute -> [Attribute] 